%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_connector_mqtt).
-behaviour(gen_server).

-export([start_connector/1, stop_connector/1]).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("lorawan_db.hrl").

-record(state, {conn, connect, subscribe, published, consumed, hier}).
-record(costa, {phase, cargs, last_connect, connect_count}).

start_connector(#connector{connid=Id}=Connector) ->
    {ok, _} = lorawan_connector_sup:start_child({mqtt, Id}, ?MODULE, [Connector]).

stop_connector(Id) ->
    lorawan_connector_sup:stop_child({mqtt, Id}).

start_link(Connector) ->
    gen_server:start_link(?MODULE, [Connector], []).

init([#connector{connid=ConnId, app=App, uri=Uri, client_id=ClientId, name=UserName, pass=Password,
        subscribe=Sub, published=Pub, consumed=Cons}=Connector]) ->
    process_flag(trap_exit, true),
    lager:debug("Connecting ~s to ~s", [ConnId, Uri]),
    ok = pg2:join({backend, App}, self()),
    self() ! connect_all,
    timer:send_interval(60*1000, ping),

    {ok, #state{
        conn=Connector,
        connect=lorawan_connector:prepare_filling([Uri, ClientId, UserName, Password]),
        subscribe=lorawan_connector:prepare_filling(Sub),
        published=lorawan_connector:prepare_filling(Pub),
        consumed=lorawan_connector:prepare_matching(Cons),
        hier=[]
    }}.

build_hierarchy(PatConn, PatSub, Nodes) ->
    lists:foldl(
        fun(Node, Hier) ->
            Vars = lorawan_admin:build(node_to_vars(Node)),
            Connect = lorawan_connector:fill_pattern(PatConn, Vars),

            Subs = proplists:get_value(Connect, Hier, []),
            Subs2 =
                if
                    PatSub == undefined; PatSub == <<>> ->
                        Subs;
                    true ->
                        Subscribe = lorawan_connector:fill_pattern(PatSub, Vars),
                        case lists:member(Subscribe, Subs) of
                            true -> Subs;
                            false -> [Subscribe | Subs]
                        end
                end,
            lists:keystore(Connect, 1, Hier, {Connect, Subs2})
        end,
        [], Nodes).

node_to_vars(#node{devaddr=DevAddr}) ->
    #{devaddr=>DevAddr}.

execute_hierarchy_updates(NewHier, CurrHier, Conn) ->
    lists:filtermap(
        fun({Connect, C, CurrSub, Costa}) ->
            case proplists:get_value(Connect, NewHier) of
                undefined ->
                    % the disconnected event will not be delivered
                    emqttc:disconnect(C),
                    false;
                NewSub ->
                    % subscribe first to not loose any message
                    case lists:subtract(NewSub, CurrSub) of
                        [] ->
                            ok;
                        Subscribe ->
                            [emqttc:subscribe(C, S, 1) || S <- Subscribe]
                    end,
                    case lists:subtract(CurrSub, NewSub) of
                        [] ->
                            ok;
                        Unsubscribe ->
                            [emqttc:unsubscribe(C, U) || U <- Unsubscribe]
                    end,
                    {true, {Connect, C, NewSub, Costa}}
            end
        end,
        CurrHier)
    ++
    lists:filtermap(
        fun({Connect, NewSub}) ->
            case not lists:keymember(Connect, 1, CurrHier) of
                true ->
                    % connect, initially using MQTT 3.1.1
                    {ok, C2, Costa2} = connect(attempt311, Connect, Conn),
                    {true, {Connect, C2, NewSub, Costa2}};
                false ->
                    % already processed
                    false
            end
        end,
        NewHier).

% initial connect
connect(Phase, [Uri, ClientId, UserName, Password], Conn) ->
    {ok, ConnUri} = http_uri:parse(binary_to_list(Uri), [{scheme_defaults, [{mqtt, 1883}, {mqtts, 8883}]}]),
    {Scheme, _UserInfo, HostName, Port, _Path, _Query} = ConnUri,
    CArgs = lists:append([
        [{host, HostName},
        {port, Port},
        {logger, warning},
        {keepalive, 0}],
        auth_args(HostName, Conn#connector.auth, ClientId, UserName, Password),
        ssl_args(Scheme, Conn)
    ]),
    {ok, C} = connect0(Phase, CArgs),
    {ok, C, #costa{phase=Phase, cargs=CArgs, connect_count=0, last_connect=calendar:universal_time()}}.

reconnect(#costa{phase=Phase, cargs=CArgs, connect_count=Count}=Costa) ->
    {ok, C} = connect0(Phase, CArgs),
    {ok, C, Costa#costa{connect_count=Count+1, last_connect=calendar:universal_time()}}.

connect0(attempt31, CArgs) ->
    emqttc:start_link([{proto_ver, 3} | CArgs]);
connect0(attempt311, CArgs) ->
    emqttc:start_link([{proto_ver, 4} | CArgs]).


handle_call(_Request, _From, State) ->
    {reply, {error, unknownmsg}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(connect_all, #state{conn=Connector, connect=PatConn, subscribe=PatSub, hier=Hier}=State) ->
    NewH = build_hierarchy(PatConn, PatSub,
        lorawan_backend_factory:nodes_with_backend(Connector#connector.app)),
    Hier2 =
        execute_hierarchy_updates(NewH, Hier, Connector),
    {noreply, State#state{hier=Hier2}};

handle_info({reconnect, Connect, NewSub, Costa}, #state{hier=Hier}=State) ->
    {ok, C, Costa2} = reconnect(Costa),
    {noreply, State#state{hier=lists:keystore(Connect, 1, Hier,
        {Connect, C, NewSub, Costa2})}};

handle_info({mqttc, C, connected}, #state{hier=Hier}=State) ->
    {Connect, C, NewSub, Costa} = lists:keyfind(C, 2, Hier),
    % make all required subscriptions
    [emqttc:subscribe(C, S, 1) || S <- NewSub],
    {noreply, State#state{hier=lists:keystore(C, 2, Hier,
        {Connect, C, NewSub, Costa#costa{phase=connected}})}};

handle_info({mqttc, _C, disconnected}, State) ->
    % no action, waiting for 'EXIT'
    {noreply, State};

handle_info({uplink, Node, Vars0}, #state{conn=#connector{format=Format},
        connect=PatConn, published=PatPub, hier=Hier}=State) ->
    % determine the connection to use
    Vars = lorawan_admin:build(maps:merge(node_to_vars(Node), Vars0)),
    Connect = lorawan_connector:fill_pattern(PatConn, Vars),
    {Connect, C, _NewSub, _Costa} = lists:keyfind(Connect, 1, Hier),
    % publish
    emqttc:publish(C,
        lorawan_connector:fill_pattern(PatPub, Vars),
        encode_uplink(Format, Vars0)),
    {noreply, State};

handle_info({publish, Topic, Payload}, State=#state{conn=Connector, consumed=Pattern}) ->
    % we assume the Topic is sufficient to determine the target
    case lorawan_connector:decode_and_downlink(Connector, Payload,
            lorawan_connector:match_vars(Topic, Pattern)) of
        ok ->
            ok;
        {error, {Object, Error}} ->
            lorawan_utils:throw_error(Object, Error);
        {error, Error} ->
            lorawan_utils:throw_error({connector, Connector#connector.connid}, Error)
    end,
    {noreply, State};

handle_info(ping, #state{hier=Hier}=State) ->
    lists:foreach(
        fun({_Connect, C, _NewSub, _Costa}) ->
            pong = emqttc:ping(C)
        end,
        Hier),
    {noreply, State};

handle_info({'EXIT', C, Error}, #state{conn=#connector{connid=ConnId}, hier=Hier}=State) ->
    {Connect, C, NewSub, #costa{phase=OldPhase, connect_count=Count}=Costa} = lists:keyfind(C, 2, Hier),
    lager:debug("Connector ~p to ~p (~p) failed: ~p (count: ~p)", [ConnId, hd(Connect), OldPhase, Error, Count]),
    case handle_reconnect(Connect, NewSub, Costa) of
        {ok, C2, Costa2} ->
            {noreply, State#state{hier=lists:keystore(Connect, 1, Hier,
                {Connect, C2, NewSub, Costa2})}};
        remove ->
            {noreply, State#state{hier=lists:keydelete(Connect, 1, Hier)}}
    end;

handle_info(Unknown, State) ->
    lager:debug("Unknown message: ~p", [Unknown]),
    {noreply, State}.

terminate(Reason, #state{conn=#connector{connid=ConnId}}) when Reason == normal; Reason == shutdown ->
    lager:debug("Connector ~s terminated: ~p", [ConnId, Reason]),
    ok;
terminate(Reason, #state{conn=#connector{connid=ConnId}}) ->
    lager:warning("Connector ~s terminated: ~p", [ConnId, Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_reconnect(Connect, NewSub, #costa{last_connect=Last, connect_count=Count} = Costa) ->
    case calendar:datetime_to_gregorian_seconds(calendar:universal_time())
            - calendar:datetime_to_gregorian_seconds(Last) of
        Diff when Diff < 30, Count > 120 ->
            % give up after 2 hours
            remove;
        Diff when Diff < 30, Count > 0 ->
            % wait, then wait even longer, but no longer than 30 sec
            {ok, _} = timer:send_after(erlang:min(Count*5000, 30000),
                {reconnect, Connect, NewSub, switch_ver(Costa#costa{connect_count=Count+1})}),
            remove;
        Diff when Diff < 30, Count == 0 ->
            % initially try to reconnect immediately
            reconnect(switch_ver(Costa#costa{connect_count=1}));
        _Diff ->
            reconnect(Costa#costa{connect_count=0})
    end.

switch_ver(#costa{phase=attempt311}=Costa) ->
    Costa#costa{phase=attempt31};
switch_ver(Costa) ->
    Costa#costa{phase=attempt311}.

encode_uplink(<<"raw">>, Vars) ->
    maps:get(data, Vars, <<>>);
encode_uplink(<<"json">>, Vars) ->
    jsx:encode(lorawan_admin:build(Vars));
encode_uplink(<<"www-form">>, Vars) ->
    lorawan_connector:form_encode(Vars).


% Microsoft Shared Access Signature
auth_args(HostName, <<"sas">>, DeviceID, KeyName, SharedKey) ->
    UserName = lists:flatten(
        io_lib:format("~s/~s/api-version=2016-11-14", [HostName, DeviceID])),
    [{client_id, DeviceID},
    {username, list_to_binary(UserName)},
    {password, list_to_binary(lorawan_connector:shared_access_token(HostName, DeviceID, KeyName, SharedKey))}];
% normal (and default)
auth_args(_HostName, _Auth, ClientId, UserName, Password) ->
    [{client_id, empty_undefined(ClientId)},
    {username, empty_undefined(UserName)},
    {password, empty_undefined(Password)}].

empty_undefined(<<"">>) -> undefined;
empty_undefined(Else) -> Else.

ssl_args(mqtt, _Conn) ->
    [];
ssl_args(mqtts, #connector{certfile=CertFile, keyfile=KeyFile})
        when is_binary(CertFile), size(CertFile) > 0, is_binary(KeyFile), size(KeyFile) > 0 ->
    [{ssl, [
        {versions, ['tlsv1.2']},
        {certfile, filename:absname(binary_to_list(CertFile))},
        {keyfile, filename:absname(binary_to_list(KeyFile))}
    ]}];
ssl_args(mqtts, #connector{}) ->
    [{ssl, [
        {versions, ['tlsv1.2']}
    ]}].

% end of file
