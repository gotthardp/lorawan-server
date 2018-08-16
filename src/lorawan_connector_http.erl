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

-record(state, {conn, pid, mref, ready, streams, publish_uplinks, publish_events, auth, nc}).

start_connector(#connector{connid=Id, received=Received}=Connector) ->
    case lorawan_connector:pattern_for_cowboy(Received) of
        undefined ->
            ok;
        error ->
            lorawan_connector:raise_failed(Id, {badarg, Received});
        Pattern ->
            lorawan_http_registry:update({http, Id},
                #{routes => [{Pattern, lorawan_connector_http_in, [Connector]}]})
    end,
    lorawan_connector_sup:start_child(Id, ?MODULE, [Connector]).

stop_connector(Id) ->
    lorawan_http_registry:delete({http, Id}),
    lorawan_connector_sup:stop_child(Id).

start_link(Connector) ->
    gen_server:start_link(?MODULE, [Connector], []).

init([#connector{connid=Id, app=App,
        publish_uplinks=PubUp, publish_events=PubEv, name=UserName, pass=Password}=Conn]) ->
    ok = pg2:join({backend, App}, self()),
    try
        {ok, ensure_gun(
            #state{conn=Conn,
                publish_uplinks=lorawan_connector:prepare_filling(PubUp),
                publish_events=lorawan_connector:prepare_filling(PubEv),
                auth=lorawan_connector:prepare_filling([UserName, Password]),
                nc=1})}
    catch
        _:Error ->
            lorawan_connector:raise_failed(Id, Error),
            {stop, shutdown}
    end.

handle_call(_Request, _From, State) ->
    {reply, {error, unknownmsg}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(nodes_changed, State) ->
    % nothing to do here
    {noreply, State};

handle_info({uplink, _Node, _Vars0}, #state{publish_uplinks=PatPub}=State)
        when PatPub == undefined; PatPub == ?EMPTY_PATTERN ->
    {noreply, State};
handle_info({uplink, _Node, Vars0}, #state{conn=Conn}=State) ->
    case ensure_connected(ensure_gun(State)) of
        {ok, State2} ->
            {noreply, handle_uplink(Vars0, State2)};
        {error, State2} ->
            lager:warning("Connector ~p not connected, uplink lost", [Conn#connector.connid]),
            {noreply, State2}
    end;

handle_info({event, _Node, _Vars0}, #state{publish_events=PatPub}=State)
        when PatPub == undefined; PatPub == ?EMPTY_PATTERN ->
    {noreply, State};
handle_info({event, _Node, Vars0}, #state{conn=Conn}=State) ->
    case ensure_connected(ensure_gun(State)) of
        {ok, State2} ->
            {noreply, handle_event(Vars0, State2)};
        {error, State2} ->
            lager:warning("Connector ~p not connected, event lost", [Conn#connector.connid]),
            {noreply, State2}
    end;

handle_info({gun_up, C, http}, State=#state{pid=C}) ->
    {noreply, State#state{ready=true}};
handle_info({gun_down, C, _Proto, _Reason, Killed, Unprocessed},
        State=#state{pid=C, streams=Streams}) ->
    {noreply, State#state{ready=false, streams=remove_list(remove_list(Streams, Killed), Unprocessed)}};
handle_info({gun_response, C, StreamRef, Fin, 401, Headers},
        State=#state{pid=C, streams=Streams}) ->
    State3 =
        case proplists:get_value(<<"www-authenticate">>, Headers) of
            undefined ->
                lager:warning("HTTP request failed: 401"),
                State;
            WWWAuthenticate ->
                {URI, Auth, ContentType, Body} = maps:get(StreamRef, Streams),
                case handle_authenticate([digest, basic], URI, Auth, Body,
                        cow_http_hd:parse_www_authenticate(WWWAuthenticate), State) of
                    {[], State2} ->
                        lager:warning("Authentication failed: ~p", [WWWAuthenticate]),
                        State2;
                    {Auth, State2} ->
                        do_publish({URI, authenticated, ContentType, Body}, Auth, State2)
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

handle_info({'DOWN', _MRef, process, C, Reason}, #state{conn=Conn, pid=C}=State) ->
    lager:warning("Connector ~s failed: ~p", [Conn#connector.connid, Reason]),
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

ensure_gun(#state{pid=Pid}=State) when is_pid(Pid) ->
    % is running
    State;
ensure_gun(#state{conn=#connector{uri= <<"http:">>}, pid=undefined}=State) ->
    % should not be running
    State;
ensure_gun(#state{conn=#connector{connid=ConnId, uri=Uri}, pid=undefined}=State) ->
    lager:debug("Connecting ~s to ~s", [ConnId, Uri]),
    {ok, ConnPid} =
        case http_uri:parse(binary_to_list(Uri), [{scheme_defaults, [{http, 80}, {https, 443}]}]) of
            {ok, {http, _UserInfo, HostName, Port, _Path, _Query}} ->
                gun:open(HostName, Port);
            {ok, {https, _UserInfo, HostName, Port, _Path, _Query}} ->
                gun:open(HostName, Port, #{transport=>ssl})
        end,
    MRef = monitor(process, ConnPid),
    State#state{pid=ConnPid, mref=MRef, ready=false, streams=#{}}.

ensure_connected(#state{ready=true}=State) ->
    {ok, State};
ensure_connected(#state{pid=undefined, ready=false}=State) ->
    {error, State};
ensure_connected(#state{conn=Conn, pid=ConnPid, mref=MRef, ready=false}=State) ->
    case gun:await_up(ConnPid, MRef) of
        {ok, _Protocol} ->
            {ok, State#state{ready=true}};
        {error, Reason} ->
            lager:debug("~s failed to connect: ~p", [Conn#connector.connid, Reason]),
            {error, State}
    end.

disconnect(undefined) ->
    ok;
disconnect(ConnPid) ->
    gun:close(ConnPid).

handle_uplink(Vars0, State) when is_list(Vars0) ->
    lists:foldl(
        fun(V0, S) -> handle_uplink(V0, S) end,
        State, Vars0);
handle_uplink(Vars0, #state{conn=#connector{format=Format}, publish_uplinks=Publish}=State) ->
    {ContentType, Body} = encode_uplink(Format, Vars0),
    send_publish(lorawan_admin:build(Vars0), Publish, ContentType, Body, State).

handle_event(Vars0, #state{publish_events=Publish}=State) ->
    Vars = lorawan_admin:build(Vars0),
    send_publish(Vars, Publish, <<"application/json">>, jsx:encode(Vars), State).

send_publish(Vars, Publish, ContentType, Body, #state{conn=Conn, auth=AuthP}=State) ->
    URI = lorawan_connector:fill_pattern(Publish, Vars),
    [User, Pass] = lorawan_connector:fill_pattern(AuthP, Vars),
    case Conn of
        #connector{auth = <<"token">>} ->
            do_publish({URI, authenticated, ContentType, Body}, [{User, Pass}], State);
        #connector{} ->
            do_publish({URI, [User, Pass], ContentType, Body}, [], State)
    end.

do_publish({URI, _Auth, ContentType, Body}=Msg, Headers, State=#state{pid=C, streams=Streams}) ->
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

handle_authenticate(_, _, authenticated, _, _, State) ->
    {[], State};
handle_authenticate([Scheme | Rest], URI, Auth, Body, WWWAuthenticate, State) ->
    case proplists:get_value(Scheme, WWWAuthenticate) of
        undefined ->
            handle_authenticate(Rest, URI, Auth, Body, WWWAuthenticate, State);
        Value ->
            handle_authenticate0(Scheme, Value, URI, Auth, Body, State)
    end;
handle_authenticate([], _, _, _, _, State) ->
    {[], State}.

handle_authenticate0(_, _, _URI, [Name, Pass], _, State)
        when Name == undefined; Pass == undefined ->
    lager:error("No credentials for HTTP authentication"),
    {[], State};
handle_authenticate0(basic, _, _, [Name, Pass], _, State) ->
    Cred = base64:encode(<<Name/binary, $:, Pass/binary>>),
    {[lorawan_http_digest:authorization_header(basic, Cred)], State};
handle_authenticate0(digest, Value, URI, [Name, Pass], Body, State=#state{nc=Nc0}) ->
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
