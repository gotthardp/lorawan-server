%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_connector_mqtt).
-behaviour(gen_server).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("lorawan.hrl").

-record(state, {cargs, phase, mqttc, ping_timer, subscribe, published, consumed}).

start_link(ConnUri, Conn) ->
    gen_server:start_link(?MODULE, [ConnUri, Conn], []).

init([ConnUri, Conn=#connector{subscribe=Sub, published=Pub, consumed=Cons}]) ->
    process_flag(trap_exit, true),
    {_Scheme, _UserInfo, HostName, Port, _Path, _Query} = ConnUri,
    lager:debug("Connecting ~s to ~s", [Conn#connector.connid, Conn#connector.uri]),
    CArgs = lists:append([
        [{host, HostName},
        {port, Port},
        {logger, warning},
        {reconnect, {4, 60, 3}},
        {keepalive, 0}],
        auth_args(ConnUri, Conn),
        ssl_args(ConnUri, Conn)
    ]),
    % initially use MQTT 3.1.1
    {ok, C} = emqttc:start_link([{proto_ver, 4} | CArgs]),
    {ok, #state{cargs=CArgs, phase=connect311, mqttc=C, subscribe=Sub,
        published=prepare_filling(Pub), consumed=prepare_matching(Cons)}}.

% Microsoft Shared Access Signature
auth_args({_Scheme, _UserInfo, HostName, _Port, _Path, _Query},
        #connector{auth= <<"sas">>, client_id=DeviceID, name=KeyName, pass=SharedKey}) ->
    UserName = lists:flatten(
        io_lib:format("~s/~s/api-version=2016-11-14", [HostName, DeviceID])),
    [{client_id, DeviceID},
    {username, list_to_binary(UserName)},
    {password, list_to_binary(shared_access_token(HostName, DeviceID, KeyName, SharedKey))}];
% normal (and default)
auth_args(_ConnUri, #connector{client_id=ClientId, name=UserName, pass=Password}) ->
    [{client_id, empty_undefined(ClientId)},
    {username, empty_undefined(UserName)},
    {password, empty_undefined(Password)}].

empty_undefined(<<"">>) -> undefined;
empty_undefined(Else) -> Else.

ssl_args({mqtt, _UserInfo, _Host, _Port, _Path, _Query}, _Conn) ->
    [];
ssl_args({mqtts, _UserInfo, _Host, _Port, _Path, _Query}, #connector{certfile=CertFile, keyfile=KeyFile})
        when is_binary(CertFile), size(CertFile) > 0, is_binary(KeyFile), size(KeyFile) > 0 ->
    [{ssl, [
        {versions, ['tlsv1.2']},
        {certfile, filename:absname(binary_to_list(CertFile))},
        {keyfile, filename:absname(binary_to_list(KeyFile))}
    ]}];
ssl_args({mqtts, _UserInfo, _Host, _Port, _Path, _Query}, #connector{}) ->
    [{ssl, [
        {versions, ['tlsv1.2']}
    ]}].

handle_call(disconnect, _From, State=#state{mqttc=C, ping_timer=Timer}) ->
    % the disconnected event will not be delivered
    emqttc:disconnect(C),
    maybe_cancel_timer(Timer),
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {stop, {error, unknownmsg}, State}.

handle_cast({publish, Msg, Vars}, State) ->
    handle_publish(Msg, Vars, State).

handle_info({mqttc, C, connected}, State=#state{mqttc=C}) ->
    handle_connect(State);
handle_info({mqttc, C, disconnected}, State=#state{mqttc=C}) ->
    handle_disconnect(State);
handle_info(ping, State) ->
    handle_ping(State);
handle_info({publish, Topic, Payload}, State) ->
    handle_consume(Topic, Payload, State);
handle_info({'EXIT', C, {shutdown, _Error}}, State=#state{cargs=CArgs, phase=connect311}) ->
    % downgrade to MQTT 3.1
    {ok, C} = emqttc:start_link([{proto_ver, 3} | CArgs]),
    {noreply, State#state{phase=connect31, mqttc=C}};
handle_info({'EXIT', C, {shutdown, Error}}, State=#state{mqttc=C, ping_timer=Timer}) ->
    maybe_cancel_timer(Timer),
    {stop, {shutdown, Error}, State};
handle_info(_Unknown, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_connect(#state{subscribe=undefined}=State) ->
    {noreply, State#state{phase=connected}};
handle_connect(#state{mqttc=C, subscribe=Topic}=State) ->
    emqttc:subscribe(C, Topic, 1),
    Timer = schedule_refresh(),
    {noreply, State#state{phase=connected, ping_timer=Timer}}.

handle_ping(State=#state{mqttc=C}) ->
    pong = emqttc:ping(C),
    Timer = schedule_refresh(),
    {noreply, State#state{ping_timer=Timer}}.

handle_disconnect(State=#state{ping_timer=Timer}) ->
    maybe_cancel_timer(Timer),
    {noreply, State#state{ping_timer=undefined}}.

schedule_refresh() ->
    erlang:send_after(60*1000, self(), ping).

maybe_cancel_timer(undefined) ->
    ok;
maybe_cancel_timer(Timer) ->
    erlang:cancel_timer(Timer).


handle_publish(Msg, Vars, State=#state{mqttc=C, published=Pattern}) ->
    emqttc:publish(C, fill_pattern(Pattern, Vars), Msg),
    {noreply, State}.

handle_consume(Topic, Msg, State=#state{consumed=Pattern}) ->
    Vars = match_pattern(Topic, Pattern),
    case lorawan_application_backend:handle_downlink(Msg, undefined,
            proplists:get_value(group, Vars),
            lorawan_mac:hex_to_binary(proplists:get_value(devaddr, Vars))) of
        ok ->
            {noreply, State};
        {error, Error} ->
            lager:error("Bad downlink ~w", [Error]),
            {noreply, State}
    end.


prepare_filling(undefined) ->
    undefined;
prepare_filling(Pattern) ->
    case re:run(Pattern, "{[^}]+}", [global]) of
        {match, [Match]} ->
            {Pattern,
                [{binary_to_existing_atom(binary:part(Pattern, Start+1, Len-2), latin1), {Start, Len}}
                    || {Start, Len} <- Match]};
        nomatch ->
            {Pattern, []}
    end.

fill_pattern({Pattern, []}, _) ->
    Pattern;
fill_pattern({Pattern, Vars}, Values) ->
    lists:foldl(
        fun({Var, Val}, Patt) ->
            case proplists:get_value(Var, Vars) of
                {Start, Len} ->
                    <<Prefix:Start/binary, _:Len/binary, Suffix/binary>> = Patt,
                    <<Prefix/binary, Val/binary, Suffix/binary>>;
                undefined ->
                    Patt
            end
        end, Pattern, Values).

prepare_matching(undefined) ->
    undefined;
prepare_matching(Pattern) ->
    EPattern = binary:replace(Pattern, <<".">>, <<"\\">>, [global, {insert_replaced, 1}]),
    case re:run(EPattern, "{[^}]+}", [global]) of
        {match, [Match]} ->
            Regex = lists:foldl(
                fun({Start, Len}, Patt) ->
                    <<Prefix:Start/binary, _:Len/binary, Suffix/binary>> = Patt,
                    <<Prefix/binary, "([a-zA-z0-9]*)", Suffix/binary>>
                end, EPattern, Match),
            {ok, MP} = re:compile(<<"^", Regex/binary, "$">>),
            {MP, [binary_to_existing_atom(binary:part(EPattern, Start+1, Len-2), latin1) || {Start, Len} <- Match]};
        nomatch ->
            {Pattern, []}
    end.

match_pattern(Topic, {Pattern, Vars}) ->
    case re:run(Topic, Pattern, [global, {capture, all, binary}]) of
        {match, [[_Head | Matches]]} ->
            lists:zip(Vars, Matches);
        nomatch ->
            undefined
    end.

% Shared Access Signature functions
% see https://docs.microsoft.com/en-us/azure/storage/storage-dotnet-shared-access-signature-part-1

shared_access_token(HostName, DeviceID, undefined, AccessKey) ->
    Res = lists:flatten(
        io_lib:format("~s/devices/~s", [HostName, DeviceID])),
    lists:flatten(
        build_access_token(Res, AccessKey));

shared_access_token(HostName, _DeviceID, KeyName, AccessKey) ->
    Res = lists:flatten(
        io_lib:format("~s/devices", [HostName])),
    lists:flatten(
        [build_access_token(Res, AccessKey), io_lib:format("&skn=~s", [KeyName])]).

build_access_token(Res0, AccessKey) ->
    build_access_token(Res0, AccessKey, 60*60*24*7). % expires in a week

build_access_token(Res0, AccessKey, Expiry) ->
    Res = http_uri:encode(Res0),
    % seconds since the UNIX epoch
    Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time())
     - calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}),
    ToSign = lists:flatten(
        io_lib:format("~s~n~B", [Res, Now+Expiry])),
    Sig = http_uri:encode(base64:encode_to_string(
        crypto:hmac(sha256, base64:decode(AccessKey), ToSign))),
    io_lib:format("SharedAccessSignature sr=~s&sig=~s&se=~B", [Res, Sig, Now+Expiry]).

-include_lib("eunit/include/eunit.hrl").

matchtst(undefined = Vars, Pattern, Topic) ->
    [?_assertEqual(Vars, match_pattern(Topic, prepare_matching(Pattern))),
    ?_assertEqual(Pattern, fill_pattern(prepare_filling(Pattern), Vars))];
matchtst(Vars, Pattern, Topic) ->
    [?_assertEqual(Vars, match_pattern(Topic, prepare_matching(Pattern))),
    ?_assertEqual(Topic, fill_pattern(prepare_filling(Pattern), Vars))].

pattern_test_()-> [
    matchtst([], <<"normal/uri">>, <<"normal/uri">>),
    matchtst(undefined, <<"normal/uri">>, <<"another/uri">>),
    matchtst([{devaddr, <<"00112233">>}], <<"{devaddr}">>, <<"00112233">>),
    matchtst([{devaddr, <<"00112233">>}], <<"prefix.{devaddr}">>, <<"prefix.00112233">>),
    matchtst([{devaddr, <<"00112233">>}], <<"{devaddr}/suffix">>, <<"00112233/suffix">>),
    matchtst([{devaddr, <<"00112233">>}], <<"prefix:{devaddr}:suffix">>, <<"prefix:00112233:suffix">>)].

% end of file
