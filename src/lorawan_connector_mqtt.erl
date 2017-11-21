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

-record(state, {connid, cargs, phase, mqttc, last_connect, connect_count,
    ping_timer, subscribe, published, consumed}).

start_link(ConnUri, Conn) ->
    gen_server:start_link(?MODULE, [ConnUri, Conn], []).

init([ConnUri, Conn=#connector{subscribe=Sub, published=Pub, consumed=Cons}]) ->
    process_flag(trap_exit, true),
    {_Scheme, _UserInfo, HostName, Port, _Path, _Query} = ConnUri,
    lager:debug("Connecting ~s to ~s", [Conn#connector.connid, Conn#connector.uri]),
    ok = syn:register(Conn#connector.connid, self()),
    CArgs = lists:append([
        [{host, HostName},
        {port, Port},
        {logger, warning},
        {keepalive, 0}],
        auth_args(ConnUri, Conn),
        ssl_args(ConnUri, Conn)
    ]),
    % initially use MQTT 3.1.1
    {ok, connect(attempt311, #state{connid=Conn#connector.connid,
        cargs=CArgs, connect_count=0, subscribe=Sub,
        published=lorawan_connector_pattern:prepare_filling(Pub),
        consumed=lorawan_connector_pattern:prepare_matching(Cons)})}.

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

handle_call({resubscribe, Sub2}, _From, State=#state{mqttc=C, subscribe=Sub1})
        when C =/= undefined, Sub1 =/= Sub2 ->
    % subscribe first to not loose any message
    emqttc:subscribe(C, Sub2, 1),
    emqttc:unsubscribe(C, Sub1),
    {reply, ok, State#state{subscribe=Sub2}};
handle_call({resubscribe, Sub2}, _From, State) ->
    % nothing to do
    {reply, ok, State#state{subscribe=Sub2}};
handle_call(_Request, _From, State) ->
    {reply, {error, unknownmsg}, State}.

handle_cast({publish, _Message}, State=#state{mqttc=undefined}) ->
    lager:warning("MQTT broker disconnected, message lost"),
    {noreply, State};
handle_cast({publish, Message}, State) ->
    handle_publish(Message, State);
handle_cast(disconnect, State=#state{mqttc=undefined}) ->
    % already disconnected
    {stop, normal, State};
handle_cast(disconnect, State=#state{mqttc=C}) ->
    % the disconnected event will not be delivered
    emqttc:disconnect(C),
    {stop, normal, State}.

handle_info({connect, Phase}, State) ->
    {noreply, connect(Phase, State)};
handle_info(ping, State) ->
    handle_ping(State);

handle_info({mqttc, C, connected}, State=#state{mqttc=C}) ->
    handle_connect(State);
handle_info({mqttc, C, disconnected}, State=#state{mqttc=C}) ->
    % no action, waiting for 'EXIT'
    {noreply, State};
handle_info({publish, Topic, Payload}, State) ->
    handle_consume(Topic, Payload, State);
handle_info({'EXIT', C, Error}, State=#state{phase=attempt311, mqttc=C}) ->
    handle_reconnect(Error, attempt31, State);
handle_info({'EXIT', C, Error}, State=#state{mqttc=C}) ->
    handle_reconnect(Error, attempt311, State);
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

handle_reconnect(Error, Phase, #state{ping_timer=Timer} = State) ->
    maybe_cancel_timer(Timer),
    handle_reconnect0(Error, Phase, State#state{mqttc=undefined, ping_timer=undefined}).

handle_reconnect0(Error, Phase, #state{connid=ConnId, phase=OldPhase, last_connect=Last, connect_count=Count} = State) ->
    lager:debug("Connector ~s(~s) failed: ~p, reconnect ~B", [ConnId, OldPhase, Error, Count]),
    case calendar:datetime_to_gregorian_seconds(calendar:universal_time())
            - calendar:datetime_to_gregorian_seconds(Last) of
        Diff when Diff < 30, Count > 120 ->
            lager:warning("Connector ~s failed to reconnect: ~p", [ConnId, Error]),
            % give up after 2 hours
            lorawan_connector_factory:disable_connector(ConnId),
            {stop, normal, State};
        Diff when Diff < 30, Count > 0 ->
            % wait, then wait even longer, but no longer than 30 sec
            {ok, _} = timer:send_after(erlang:min(Count*5000, 30000), {connect, Phase}),
            {noreply, State#state{connect_count=Count+1}};
        Diff when Diff < 30, Count == 0 ->
            % initially try to reconnect immediately
            {noreply, connect(Phase, State#state{connect_count=1})};
        _Diff ->
            {noreply, connect(Phase, State#state{connect_count=0})}
    end.

connect(Phase, #state{cargs=CArgs} = State) ->
    {ok, C} = connect0(Phase, CArgs),
    State#state{phase=Phase, mqttc=C, last_connect=calendar:universal_time()}.

connect0(attempt31, CArgs) ->
    emqttc:start_link([{proto_ver, 3} | CArgs]);
connect0(attempt311, CArgs) ->
    emqttc:start_link([{proto_ver, 4} | CArgs]).

handle_connect(#state{subscribe=undefined}=State) ->
    {noreply, State#state{phase=connected}};
handle_connect(#state{mqttc=C, subscribe=Topic}=State) ->
    emqttc:subscribe(C, Topic, 1),
    Timer = schedule_refresh(),
    {noreply, State#state{phase=connected, ping_timer=Timer}}.

handle_ping(State=#state{mqttc=undefined}) ->
    % ping timer expired, but meanwhile the client crashed
    % wait for reconnection
    {noreply, State};
handle_ping(State=#state{mqttc=C}) ->
    pong = emqttc:ping(C),
    Timer = schedule_refresh(),
    {noreply, State#state{ping_timer=Timer}}.

schedule_refresh() ->
    erlang:send_after(60*1000, self(), ping).

maybe_cancel_timer(undefined) ->
    ok;
maybe_cancel_timer(Timer) ->
    _ = erlang:cancel_timer(Timer),
    ok.


handle_publish({_ContentType, Msg, Vars}, State=#state{mqttc=C, published=Pattern}) ->
    emqttc:publish(C, lorawan_connector_pattern:fill_pattern(Pattern, lorawan_admin:build(Vars)), Msg),
    {noreply, State}.

handle_consume(Topic, Msg, State=#state{consumed=Pattern}) ->
    case lorawan_application_backend:handle_downlink(Msg, undefined,
            lorawan_connector_pattern:match_vars(Topic, Pattern)) of
        ok ->
            ok;
        {error, {Object, Error}} ->
            lorawan_utils:throw_error(Object, Error);
        {error, Error} ->
            lorawan_utils:throw_error(server, Error)
    end,
    {noreply, State}.

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

% end of file
