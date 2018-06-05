%
% Copyright (c) 2016-2018 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_connector_http).
-behaviour(gen_server).

-export([start_connector/1, stop_connector/1]).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("lorawan_db.hrl").

-record(state, {conn, pid, streams, publish_uplinks, publish_events, auth, nc}).

start_connector(#connector{connid=Id, received=Received}=Connector) ->
    case lorawan_connector:pattern_for_cowboy(Received) of
        undefined ->
            ok;
        error ->
            lorawan_connector:raise_failed(Id, {badarg, Received});
        Pattern ->
            lorawan_http_registry:update_routes({http, Id},
                [{Pattern, lorawan_connector_http_in, [Connector]}])
    end,
    lorawan_connector_sup:start_child(Id, ?MODULE, [Connector]).

stop_connector(Id) ->
    lorawan_http_registry:delete_routes({http, Id}),
    lorawan_connector_sup:stop_child(Id).

start_link(Connector) ->
    gen_server:start_link(?MODULE, [Connector], []).

init([#connector{connid=Id, app=App, publish_uplinks=PubUp, publish_events=PubEv}=Conn]) ->
    ok = pg2:join({backend, App}, self()),
    self() ! connect,
    try
        {ok, #state{conn=Conn, streams=#{},
            publish_uplinks=lorawan_connector:prepare_filling(PubUp),
            publish_events=lorawan_connector:prepare_filling(PubEv)
        }}
    catch
        _:Error ->
            lorawan_connector:raise_failed(Id, Error),
            {stop, shutdown}
    end.

handle_call(_Request, _From, State) ->
    {reply, {error, unknownmsg}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(connect, #state{conn=Connector}=State) ->
    case connect(Connector) of
        {ok, ConnPid} ->
            {noreply, State#state{pid=ConnPid}};
        {error, _} ->
            % will try to reconnect upon uplink
            {noreply, State#state{pid=undefined}}
    end;
handle_info(nodes_changed, State) ->
    % nothing to do here
    {noreply, State};

handle_info({uplink, _Node, _Vars0}, #state{publish_uplinks=PatPub}=State)
        when PatPub == undefined; PatPub == ?EMPTY_PATTERN ->
    {noreply, State};
handle_info({uplink, _Node, Vars0}, #state{conn=Connector, pid=undefined}=State) ->
    case connect(Connector) of
        {ok, ConnPid} ->
            {noreply, handle_uplink(Vars0, State#state{pid=ConnPid})};
        {error, _} ->
            % will try to reconnect upon next uplink
            {noreply, State#state{pid=undefined}}
    end;
handle_info({uplink, _Node, Vars0}, #state{pid=ConnPid}=State) when is_pid(ConnPid) ->
    {noreply, handle_uplink(Vars0, State)};

handle_info({event, _Node, _Vars0}, #state{publish_events=PatPub}=State)
        when PatPub == undefined; PatPub == ?EMPTY_PATTERN ->
    {noreply, State};
handle_info({event, _Node, Vars0}, #state{conn=Connector, pid=undefined}=State) ->
    case connect(Connector) of
        {ok, ConnPid} ->
            {noreply, handle_event(Vars0, State#state{pid=ConnPid})};
        {error, _} ->
            {noreply, State#state{pid=undefined}}
    end;
handle_info({event, _Node, Vars0}, #state{pid=ConnPid}=State) when is_pid(ConnPid) ->
    {noreply, handle_event(Vars0, State)};

handle_info({gun_up, C, http}, State=#state{pid=C}) ->
    {noreply, State};
handle_info({gun_down, C, _Proto, _Reason, Killed, Unprocessed},
        State=#state{pid=C, streams=Streams}) ->
    {noreply, State#state{streams=remove_list(remove_list(Streams, Killed), Unprocessed)}};
handle_info({gun_response, C, StreamRef, Fin, 401, Headers},
        State=#state{pid=C, streams=Streams}) ->
    State3 =
        case proplists:get_value(<<"www-authenticate">>, Headers) of
            undefined ->
                lager:warning("HTTP request failed: 401"),
                State;
            WWWAuthenticate ->
                case maps:get(StreamRef, Streams) of
                    {initial, URI, ContentType, Body} ->
                        case handle_authenticate([digest, basic], URI, Body,
                                cow_http_hd:parse_www_authenticate(WWWAuthenticate), State) of
                            {[], State2} ->
                                lager:warning("Unsupported authentication mechanism: ~p", [WWWAuthenticate]),
                                State2;
                            {Auth, State2} ->
                                do_publish({authorized, URI, ContentType, Body}, Auth, State2)
                        end;
                    {authorized, _URI, _ContentType, _Body} ->
                        lager:warning("HTTP authentication failed"),
                        State
                end
        end,
    {noreply, fin_stream(StreamRef, Fin, State3)};
handle_info({gun_response, C, StreamRef, Fin, Status, _Headers},
        State=#state{pid=C, streams=Streams}) ->
    if
        Status < 300 ->
            ok;
        Status >= 300 ->
            {_, URI, _, _} = maps:get(StreamRef, Streams),
            lager:debug("HTTP request to ~p failed: ~B", [URI, Status]),
            ok
    end,
    {noreply, fin_stream(StreamRef, Fin, State)};
handle_info({gun_data, C, StreamRef, Fin, _Data}, State=#state{pid=C}) ->
    {noreply, fin_stream(StreamRef, Fin, State)};

handle_info({'DOWN', _MRef, process, C, Reason}, #state{conn=#connector{connid=ConnId}, pid=C}=State) ->
    lager:warning("Connector ~s disconnected: ~p", [ConnId, Reason]),
    % will try to reconnect when new uplink arrives
    {noreply, State#state{pid=undefined}};
handle_info(Unknown, State) ->
    lager:debug("Unknown message: ~p", [Unknown]),
    {noreply, State}.

terminate(normal, #state{conn=#connector{connid=ConnId}, pid=C}) ->
    lager:debug("Connector ~s terminated: normal", [ConnId]),
    disconnect(C);
terminate(Reason, #state{conn=#connector{connid=ConnId}, pid=C}) ->
    lager:warning("Connector ~s terminated: ~p", [ConnId, Reason]),
    disconnect(C).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

connect(#connector{connid=ConnId, uri=Uri}) ->
    lager:debug("Connecting ~s to ~s", [ConnId, Uri]),
    {ok, ConnPid} =
        case http_uri:parse(binary_to_list(Uri), [{scheme_defaults, [{http, 80}, {https, 443}]}]) of
            {ok, {http, _UserInfo, HostName, Port, _Path, _Query}} ->
                gun:open(HostName, Port);
            {ok, {https, _UserInfo, HostName, Port, _Path, _Query}} ->
                gun:open(HostName, Port, #{transport=>ssl})
        end,
    MRef = monitor(process, ConnPid),
    case gun:await_up(ConnPid, 5000, MRef) of
        {ok, Protocol} ->
            lager:debug("~s connected to ~s (~p)", [ConnId, Uri, Protocol]),
            {ok, ConnPid};
        {error, Reason} ->
            demonitor(MRef),
            lager:error("~s failed to connect to ~p ~p", [ConnId, Uri, Reason]),
            {error, Reason}
    end.

disconnect(undefined) ->
    ok;
disconnect(ConnPid) ->
    gun:close(ConnPid).

handle_uplink(Vars0, State) when is_list(Vars0) ->
    lists:foldl(
        fun(V0, S) -> handle_uplink(V0, S) end,
        State, Vars0);
handle_uplink(Vars0, #state{conn=#connector{format=Format}, publish_uplinks=Pattern}=State) ->
    {ContentType, Body} = encode_uplink(Format, Vars0),
    do_publish(
        {initial,
            lorawan_connector:fill_pattern(Pattern, lorawan_admin:build(Vars0)),
            ContentType, Body},
        [], State).

handle_event(Vars0, #state{publish_events=Pattern}=State) ->
    Vars = lorawan_admin:build(Vars0),
    do_publish(
        {initial,
            lorawan_connector:fill_pattern(Pattern, Vars),
            <<"application/json">>, jsx:encode(Vars)},
        [], State).

do_publish({_Type, URI, ContentType, Body}=Msg, Headers, State=#state{pid=C, streams=Streams}) ->
    StreamRef = gun:post(C, URI,
        [{<<"content-type">>, ContentType} | Headers], Body),
    State#state{streams=maps:put(StreamRef, Msg, Streams)}.

fin_stream(StreamRef, fin, State=#state{streams=Streams}) ->
    State#state{streams=maps:remove(StreamRef, Streams)};
fin_stream(_StreamRef, nofin, State) ->
    State.

remove_list(Map, List) ->
    lists:foldl(
        fun(Item, Map2) -> maps:remove(Item, Map2) end,
        Map, List).

encode_uplink(<<"raw">>, Vars) ->
    {<<"application/octet-stream">>, maps:get(data, Vars, <<>>)};
encode_uplink(<<"json">>, Vars) ->
    {<<"application/json">>, jsx:encode(lorawan_admin:build(Vars))};
encode_uplink(<<"www-form">>, Vars) ->
    {<<"application/x-www-form-urlencoded">>, lorawan_connector:form_encode(Vars)}.

handle_authenticate([Scheme | Rest], URI, Body, WWWAuthenticate, State) ->
    case proplists:get_value(Scheme, WWWAuthenticate) of
        undefined ->
            handle_authenticate(Rest, URI, Body, WWWAuthenticate, State);
        Value ->
            handle_authenticate0(Scheme, Value, URI, Body, State)
    end;
handle_authenticate([], _URI, _Body, _WWWAuthenticate, State) ->
    {[], State}.

handle_authenticate0(_Any, _Value, _URI, _Body, State=#state{auth={Name, Pass}})
        when Name == undefined; Pass == undefined ->
    lager:error("No credentials for HTTP authentication"),
    {[], State};
handle_authenticate0(basic, _Value, _URI, _Body, State=#state{auth={Name, Pass}}) ->
    Cred = base64:encode(<<Name/binary, $:, Pass/binary>>),
    {[lorawan_http_digest:authorization_header(basic, Cred)], State};
handle_authenticate0(digest, Value, URI, Body, State=#state{auth={Name, Pass}, nc=Nc0}) ->
    Realm = proplists:get_value(<<"realm">>, Value, <<>>),
    Nonce = proplists:get_value(<<"nonce">>, Value, <<>>),
    Opaque = proplists:get_value(<<"opaque">>, Value, <<>>),
    case proplists:get_value(<<"qop">>, Value) of
        undefined ->
            Response = lorawan_http_digest:response(<<"POST">>, URI, Body, {Name, Realm, Pass}, Nonce),
            {[lorawan_http_digest:authorization_header(digest, [{<<"username">>, Name}, {<<"realm">>, Realm},
                {<<"nonce">>, Nonce}, {<<"uri">>, URI}, {<<"algorithm">>, <<"MD5">>},
                {<<"response">>, Response}, {<<"opaque">>, Opaque}])], State};
        Qop0 ->
            [Qop|_] = binary:split(Qop0, [<<",">>], [global]),
            Nc = lorawan_http_digest:nc(Nc0),
            CNonce = lorawan_http_digest:nonce(4),
            Response = lorawan_http_digest:response(<<"POST">>, URI, Body, {Name, Realm, Pass}, Nonce, Nc, CNonce, Qop),
            {[lorawan_http_digest:authorization_header(digest, [{<<"username">>, Name}, {<<"realm">>, Realm},
                {<<"nonce">>, Nonce}, {<<"uri">>, URI}, {<<"algorithm">>, <<"MD5">>},
                {<<"response">>, Response}, {<<"opaque">>, Opaque}, {<<"qop">>, Qop},
                {<<"nc">>, Nc}, {<<"cnonce">>, CNonce}])], State#state{nc=Nc0+1}}
    end.

% end of file
