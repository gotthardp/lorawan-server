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
    {[authorization_header(basic, Cred)], State};
handle_authenticate0(digest, Value, URI, Body, State=#state{auth={Name, Pass}, nc=Nc0}) ->
    Realm = proplists:get_value(<<"realm">>, Value, <<>>),
    Nonce = proplists:get_value(<<"nonce">>, Value, <<>>),
    Opaque = proplists:get_value(<<"opaque">>, Value, <<>>),
    case proplists:get_value(<<"qop">>, Value) of
        undefined ->
            Response = digest_response(<<"POST">>, URI, Body, Name, Pass, Realm, Nonce),
            {[authorization_header(digest, [{<<"username">>, Name}, {<<"realm">>, Realm},
                {<<"nonce">>, Nonce}, {<<"uri">>, URI}, {<<"algorithm">>, <<"MD5">>},
                {<<"response">>, Response}, {<<"opaque">>, Opaque}])], State};
        Qop0 ->
            [Qop|_] = binary:split(Qop0, [<<",">>], [global]),
            Nc = integer_to_hex(Nc0, 8),
            CNonce = binary_to_hex(crypto:strong_rand_bytes(4)),
            Response = digest_response(<<"POST">>, URI, Body, Name, Pass, Realm, Nonce, Nc, CNonce, Qop),
            {[authorization_header(digest, [{<<"username">>, Name}, {<<"realm">>, Realm},
                {<<"nonce">>, Nonce}, {<<"uri">>, URI}, {<<"algorithm">>, <<"MD5">>},
                {<<"response">>, Response}, {<<"opaque">>, Opaque}, {<<"qop">>, Qop},
                {<<"nc">>, Nc}, {<<"cnonce">>, CNonce}])], State#state{nc=Nc0+1}}
    end.

authorization_header(Scheme, Cred) ->
    {<<"authorization">>, authorization_header0(Scheme, Cred)}.

authorization_header0(basic, Cred) ->
    <<"Basic ", Cred/binary>>;
authorization_header0(digest, Params) ->
    <<"Digest ", (encode_params(<<>>, Params))/binary>>.

encode_params(<<>>, [First | Rest]) ->
    encode_params(encode_param(First), Rest);
encode_params(Acc, [First | Rest]) ->
    encode_params(<<Acc/binary, ", ", (encode_param(First))/binary>>, Rest);
encode_params(Acc, []) ->
    Acc.

encode_param({Name, Value})
        when Name == <<"algorithm">>; Name == <<"qop">>; Name == <<"nc">> ->
    <<Name/binary, $=, Value/binary>>;
encode_param({Name, Value}) ->
    <<Name/binary, $=, $", Value/binary, $">>.

digest_response(Method, URI, _Body, User, Password, Realm, Nonce) ->
    HA1 = binary_to_hex(crypto:hash(md5,
        <<User/binary, $:, Realm/binary, $:, Password/binary>>)),
    HA2 = binary_to_hex(crypto:hash(md5,
        <<Method/binary, $:, URI/binary>>)),
    binary_to_hex(crypto:hash(md5,
        <<HA1/binary, $:, Nonce/binary, $:, HA2/binary>>)).

digest_response(Method, URI, Body, User, Password, Realm, Nonce, Nc, CNonce, Qop) ->
    HA1 = binary_to_hex(crypto:hash(md5,
        <<User/binary, $:, Realm/binary, $:, Password/binary>>)),
    HA2 = binary_to_hex(crypto:hash(md5,
        case Qop of
            <<"auth-int">> ->
                BodyHash = binary_to_hex(crypto:hash(md5, Body)),
                <<Method/binary, $:, URI/binary, $:, BodyHash/binary>>;
            _Else ->
                <<Method/binary, $:, URI/binary>>
        end)),
    binary_to_hex(crypto:hash(md5,
        <<HA1/binary, $:, Nonce/binary, $:, Nc/binary, $:, CNonce/binary, $:, Qop/binary, $:, HA2/binary>>)).

-include_lib("eunit/include/eunit.hrl").

digest_test_() -> [
    ?_assertEqual(<<"6629fae49393a05397450978507c4ef1">>,
        digest_response(<<"GET">>, <<"/dir/index.html">>, <<>>,
            <<"Mufasa">>, <<"Circle Of Life">>, <<"testrealm@host.com">>,
            <<"dcd98b7102dd2f0e8b11d0f600bfb0c093">>,
            <<"00000001">>, <<"0a4f113b">>, <<"auth">>))].

integer_to_hex(Num, Len) ->
    list_to_binary(string:right(string:to_lower(integer_to_list(Num,16)), Len, $0)).

binary_to_hex(Id) ->
    << <<Y>> || <<X:4>> <= Id, Y <- string:to_lower(integer_to_list(X,16))>>.

% end of file
