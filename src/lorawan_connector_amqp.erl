%
% Copyright (c) 2016-2018 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_connector_amqp).
-behaviour(gen_server).

-export([start_connector/1, stop_connector/1]).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("lorawan_db.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-record(state, {conn, cpid, subc, publish_uplinks, publish_events, received}).

start_connector(#connector{connid=Id}=Connector) ->
    lorawan_connector_sup:start_child({amqp, Id}, ?MODULE, [Connector]).

stop_connector(Id) ->
    lorawan_connector_sup:stop_child({amqp, Id}).

start_link(Connector) ->
    gen_server:start_link(?MODULE, [Connector], []).

init([#connector{connid=Id, app=App, publish_uplinks=PubUp, publish_events=PubEv, received=Cons}=Connector]) ->
    process_flag(trap_exit, true),
    ok = pg2:join({backend, App}, self()),
    self() ! connect,
    try
        {ok, #state{
            conn=Connector,
            publish_uplinks=lorawan_connector:prepare_filling(PubUp),
            publish_events=lorawan_connector:prepare_filling(PubEv),
            received=lorawan_connector:prepare_matching(Cons)
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

handle_info(connect, #state{conn=#connector{connid=ConnId, uri=Uri, name=UserName, pass=Password}}=State) ->
    lager:debug("Connecting ~s to ~s", [ConnId, Uri]),
    case amqp_uri:parse(Uri) of
        {ok, Params} ->
            case amqp_connection:start(
                    Params#amqp_params_network{username=ensure_binary(UserName), password=ensure_binary(Password)}) of
                {ok, Connection} ->
                    erlang:monitor(process, Connection),
                    self() ! subscribe,
                    {noreply, State#state{cpid=Connection}};
                {error, Error} ->
                    lorawan_connector:raise_failed(ConnId, {network, Error}),
                    {stop, shutdown, State}
            end;
        {error, Error} ->
            lorawan_connector:raise_failed(ConnId, {badarg, Error}),
            {stop, shutdown, State}
    end;

handle_info(subscribe, #state{conn=#connector{subscribe=Sub}, cpid=Connection}=State)
        when is_pid(Connection), is_binary(Sub), byte_size(Sub) > 0 ->
    {ok, SubChannel} = amqp_connection:open_channel(Connection),
    erlang:monitor(process, SubChannel),
    handle_subscribe(State#state{subc=SubChannel});
handle_info(subscribe, State) ->
    {noreply, State#state{subc=undefined}};

handle_info(#'basic.cancel'{}, State) ->
    handle_subscribe(State);

handle_info({#'basic.deliver'{delivery_tag=Tag, exchange=Exchange, routing_key=RoutingKey}, Message},
        #state{conn=Connector, subc=SubChannel, received=Pattern}=State) ->
    amqp_channel:cast(SubChannel, #'basic.ack'{delivery_tag = Tag}),
    Topic =
        case Exchange of
            <<"amq.topic">> -> RoutingKey;
            EX -> <<EX/binary, $/, RoutingKey/binary>>
        end,
    #amqp_msg{payload = Payload} = Message,
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

handle_info({'basic.consume_ok', _Tag}, State) ->
    {noreply, State};

handle_info(nodes_changed, State) ->
    % nothing to do here
    {noreply, State};

handle_info({uplink, _Node, _Vars0}, #state{publish_uplinks=PatPub}=State)
        when PatPub == undefined; PatPub == ?EMPTY_PATTERN ->
    {noreply, State};
handle_info({uplink, _Node, Vars0},
        #state{conn=#connector{format=Format}, cpid=Connection, publish_uplinks=PatPub}=State) ->
    {ok, PubChannel} = amqp_connection:open_channel(Connection),
    publish_uplinks(PubChannel, Format, PatPub, Vars0),
    amqp_channel:close(PubChannel),
    {noreply, State};

handle_info({event, _Node, _Vars0}, #state{publish_events=PatPub}=State)
        when PatPub == undefined; PatPub == ?EMPTY_PATTERN ->
    {noreply, State};
handle_info({event, _Node, Vars0},
        #state{cpid=Connection, publish_events=PatPub}=State) ->
    {ok, PubChannel} = amqp_connection:open_channel(Connection),
    publish_event(PubChannel, PatPub, Vars0),
    amqp_channel:close(PubChannel),
    {noreply, State};

handle_info({'DOWN', _Ref, process, Connection, Reason}, #state{cpid=Connection}=State) ->
    lager:warning("Connection ~p", [Reason]),
    self() ! connect,
    {noreply, State#state{cpid=undefined}};
handle_info({'DOWN', _Ref, process, SubChannel, {shutdown, {connection_closing, _}}}, #state{subc=SubChannel}=State) ->
    % will subscribe after reconnecting
    {noreply, State#state{subc=undefined}};
handle_info({'DOWN', _Ref, process, SubChannel, Reason}, #state{subc=SubChannel}=State) ->
    lager:warning("Channel ~p", [Reason]),
    self() ! subscribe,
    {noreply, State#state{subc=undefined}};

handle_info(Unknown, State) ->
    lager:debug("Unknown message: ~p", [Unknown]),
    {noreply, State}.

terminate(Reason, #state{conn=Connector}=State) ->
    log_termination(Reason, Connector),
    stop_connection(State).

log_termination(Reason, #connector{connid=ConnId})
        when Reason == normal; Reason == shutdown ->
    lager:debug("Connector ~s terminated: ~p", [ConnId, Reason]);
log_termination(Reason, #connector{connid=ConnId}) ->
    lager:warning("Connector ~s terminated: ~p", [ConnId, Reason]).

stop_connection(#state{cpid=undefined}) ->
    ok;
stop_connection(#state{cpid=Pid}) ->
    amqp_connection:close(Pid),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_subscribe(#state{conn=#connector{connid=ConnId, subscribe=Sub}, subc=SubChannel}=State) ->
    lager:debug("Subscribing ~s to ~s", [ConnId, Sub]),
    {Exchange, RoutingKey} =
        case binary:split(Sub, <<$/>>) of
            [EX, RK | _] ->
                {EX, RK};
            [RK] ->
                {<<"amq.topic">>, RK}
        end,
    try amqp_channel:call(SubChannel, #'queue.declare'{auto_delete = true}) of
        #'queue.declare_ok'{queue = Queue} ->
            BindCmd = #'queue.bind'{queue = Queue,
                exchange = Exchange, routing_key = RoutingKey},
            try amqp_channel:call(SubChannel, BindCmd) of
                #'queue.bind_ok'{} ->
                    SubCmd = #'basic.consume'{queue = Queue},
                    #'basic.consume_ok'{} = amqp_channel:call(SubChannel, SubCmd),
                    {noreply, State#state{subc=SubChannel}}
                catch _:{{shutdown,{_,_,Error}},_} ->
                    lorawan_connector:raise_failed(ConnId, {topic, Error}),
                    {stop, shutdown, State}
            end
        catch _:{{shutdown,{_,_,Error}},_} ->
            lorawan_connector:raise_failed(ConnId, {topic, Error}),
            {stop, shutdown, State}
    end.

publish_uplinks(PubChannel, Format, PatPub, Vars0) when is_list(Vars0) ->
    lists:foreach(
        fun(V0) -> publish_uplink(PubChannel, Format, PatPub, V0) end,
        Vars0);
publish_uplinks(PubChannel, Format, PatPub, Vars0) when is_map(Vars0) ->
    publish_uplink(PubChannel, Format, PatPub, Vars0).

publish_uplink(PubChannel, Format, PatPub, Vars0) ->
    amqp_channel:cast(PubChannel, basic_publish(PatPub, lorawan_admin:build(Vars0)),
        #amqp_msg{payload = encode_uplink(Format, Vars0)}).

publish_event(PubChannel, PatPub, Vars0) ->
    Vars = lorawan_admin:build(Vars0),
    amqp_channel:cast(PubChannel, basic_publish(PatPub, Vars0),
        #amqp_msg{payload = jsx:encode(Vars)}).

basic_publish(PatPub, Vars) ->
    {Exchange, RoutingKey} =
        case binary:split(lorawan_connector:fill_pattern(PatPub, Vars), <<$/>>) of
            [EX, RK | _] ->
                {EX, RK};
            [RK] ->
                {<<"amq.topic">>, RK}
        end,
    #'basic.publish'{exchange = Exchange, routing_key = RoutingKey}.

encode_uplink(<<"raw">>, Vars) ->
    maps:get(data, Vars, <<>>);
encode_uplink(<<"json">>, Vars) ->
    jsx:encode(lorawan_admin:build(Vars));
encode_uplink(<<"www-form">>, Vars) ->
    lorawan_connector:form_encode(Vars).

ensure_binary(undefined) ->
    <<>>;
ensure_binary(Value) ->
    Value.

% end of file
