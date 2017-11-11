%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_connector_http).
-behaviour(gen_server).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("lorawan.hrl").

-record(state, {connid, pid, published, auth, nc, streams}).

start_link(ConnUri, Conn) ->
    gen_server:start_link(?MODULE, [ConnUri, Conn], []).

init([ConnUri, Conn=#connector{published=Pub, name=Name, pass=Pass}]) ->
    lager:debug("Connecting ~s to ~s", [Conn#connector.connid, Conn#connector.uri]),
    ok = syn:register(Conn#connector.connid, self()),
    {ok, ConnPid} =
        case ConnUri of
            {http, _UserInfo, HostName, Port, _Path, _Query} ->
                gun:open(HostName, Port);
            {https, _UserInfo, HostName, Port, _Path, _Query} ->
                gun:open(HostName, Port, #{transport=>ssl})
        end,
    monitor(process, ConnPid),
    {ok, #state{connid=Conn#connector.connid, pid=ConnPid,
        published=lorawan_connector_pattern:prepare_filling(Pub),
        auth={Name, Pass}, nc=1, streams=#{}}}.

handle_call({resubscribe, _Sub2}, _From, State) ->
    % TODO: do something about subscriptions
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknownmsg}, State}.

handle_cast({publish, Message}, State) ->
    handle_publish(Message, State);
handle_cast(disconnect, State=#state{pid=undefined}) ->
    % already disconnected
    {stop, normal, State};
handle_cast(disconnect, State=#state{pid=C}) ->
    gun:close(C),
    {stop, normal, State}.

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
handle_info({gun_response, C, StreamRef, Fin, Status, _Headers}, State=#state{pid=C}) ->
    if
        Status < 300 ->
            ok;
        Status >= 300 ->
            lager:debug("HTTP request failed: ~B", [Status]),
            ok
    end,
    {noreply, fin_stream(StreamRef, Fin, State)};
handle_info({gun_data, C, StreamRef, Fin, _Data}, State=#state{pid=C}) ->
    {noreply, fin_stream(StreamRef, Fin, State)};
handle_info({'DOWN', _MRef, process, C, {gone, Error}}, State=#state{connid=ConnId, pid=C}) ->
    lager:warning("Connector ~s failed to reconnect: ~p", [ConnId, Error]),
    lorawan_connector_factory:disable_connector(ConnId),
    {stop, normal, State};
handle_info({'DOWN', _MRef, process, C, Reason}, State=#state{pid=C}) ->
    {stop, Reason, State};
handle_info(Unknown, State) ->
    lager:debug("Unknown message: ~p", [Unknown]),
    {noreply, State}.

terminate(normal, #state{connid=ConnId}) ->
    lager:debug("Connector ~s terminated: normal", [ConnId]),
    ok;
terminate(Reason, #state{connid=ConnId}) ->
    lager:warning("Connector ~s terminated: ~p", [ConnId, Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_publish({ContentType, Body, Vars}, State=#state{published=Pattern}) ->
    {noreply, do_publish(
        {initial,
            lorawan_connector_pattern:fill_pattern(Pattern, lorawan_admin:build(Vars)),
            ContentType, Body},
        [], State)}.

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
