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

-record(state, {conn, cpid, subc, published, consumed}).

start_connector(#connector{connid=Id}=Connector) ->
    {ok, _} = lorawan_connector_sup:start_child({amqp, Id}, ?MODULE, [Connector]).

stop_connector(Id) ->
    lorawan_connector_sup:stop_child({amqp, Id}).

start_link(Connector) ->
    gen_server:start_link(?MODULE, [Connector], []).

init([#connector{connid=ConnId, app=App, uri=Uri, published=Pub, consumed=Cons}=Connector]) ->
    process_flag(trap_exit, true),
    lager:debug("Connecting ~s to ~s", [ConnId, Uri]),
    ok = pg2:join({backend, App}, self()),
    self() ! connect,

    {ok, #state{
        conn=Connector,
        published=lorawan_connector:prepare_filling(Pub),
        consumed=lorawan_connector:prepare_matching(Cons)
    }}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknownmsg}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(connect, #state{conn=#connector{uri=Uri, name=UserName, pass=Password}}=State) ->
    case amqp_uri:parse(Uri) of
        {ok, Params} ->
            case amqp_connection:start(
                    Params#amqp_params_network{username=UserName, password=Password}) of
                {ok, Connection} ->
                    erlang:monitor(process, Connection),
                    self() ! subscribe,
                    {noreply, State#state{cpid=Connection}};
                {error, Error} ->
                    lager:error("Connector failed: ~p", [Error]),
                    {stop, shutdown, State}
            end;
        {error, Error} ->
            lager:error("Connector failed: ~p", [Error]),
            {stop, shutdown, State}
    end;

handle_info(subscribe, #state{conn=#connector{subscribe=Sub}, cpid=Connection}=State)
        when is_pid(Connection), is_binary(Sub), byte_size(Sub) > 0 ->
    {ok, SubChannel} = amqp_connection:open_channel(Connection),
    erlang:monitor(process, SubChannel),
    #'queue.declare_ok'{queue = Queue} =
        amqp_channel:call(SubChannel, #'queue.declare'{auto_delete = true}),
    {Exchange, RoutingKey} =
        case binary:split(Sub, <<$/>>) of
            [EX, RK | _] ->
                {EX, RK};
            [RK] ->
                {<<"amq.topic">>, RK}
        end,
    BindCmd = #'queue.bind'{queue = Queue,
        exchange = Exchange, routing_key = RoutingKey},
    #'queue.bind_ok'{} = amqp_channel:call(SubChannel, BindCmd),

    SubCmd = #'basic.consume'{queue = Queue},
    #'basic.consume_ok'{} = amqp_channel:call(SubChannel, SubCmd),
    {noreply, State#state{subc=SubChannel}};
handle_info(subscribe, State) ->
    {noreply, State#state{subc=undefined}};

handle_info({'basic.consume_ok', _Tag}, State) ->
    {noreply, State};

handle_info({uplink, _Node, Vars0},
        #state{conn=#connector{format=Format}, cpid=Connection, published=PatPub}=State) ->
    {ok, PubChannel} = amqp_connection:open_channel(Connection),
    publish(PubChannel, Format, PatPub, Vars0),
    amqp_channel:close(PubChannel),
    {noreply, State};

handle_info({'DOWN', _Ref, process, Connection, {Reason, Info}}, #state{cpid=Connection}=State) ->
    lager:warning("Connection ~p: ~p", [Reason, Info]),
    self() ! connect,
    {noreply, State#state{cpid=undefined}};
handle_info({'DOWN', _Ref, process, SubChannel, {shutdown, {connection_closing, _}}}, #state{subc=SubChannel}=State) ->
    % will subscribe after reconnecting
    {noreply, State#state{subc=undefined}};
handle_info({'DOWN', _Ref, process, SubChannel, {Reason, Info}}, #state{subc=SubChannel}=State) ->
    lager:warning("Channel ~p: ~p", [Reason, Info]),
    self() ! subscribe,
    {noreply, State#state{subc=undefined}};

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

publish(PubChannel, Format, PatPub, Vars0) when is_list(Vars0) ->
    lists:foreach(
        fun(V0) -> publish(PubChannel, Format, PatPub, V0) end,
        Vars0);
publish(PubChannel, Format, PatPub, Vars0) when is_map(Vars0) ->
    {Exchange, RoutingKey} =
        case binary:split(lorawan_connector:fill_pattern(PatPub, lorawan_admin:build(Vars0)), <<$/>>) of
            [EX, RK | _] ->
                {EX, RK};
            [RK] ->
                {<<"amq.topic">>, RK}
        end,
    Publish = #'basic.publish'{exchange = Exchange, routing_key = RoutingKey},
    amqp_channel:cast(PubChannel, Publish,
        #amqp_msg{payload = encode_uplink(Format, Vars0)}).

encode_uplink(<<"raw">>, Vars) ->
    maps:get(data, Vars, <<>>);
encode_uplink(<<"json">>, Vars) ->
    jsx:encode(lorawan_admin:build(Vars));
encode_uplink(<<"www-form">>, Vars) ->
    lorawan_connector:form_encode(Vars).

% end of file
