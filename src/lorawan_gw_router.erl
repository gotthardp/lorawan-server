%
% Copyright (c) 2016-2018 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_gw_router).
-behaviour(gen_server).

-export([start_link/0]).
-export([alive/3, network_delay/2, report/2, uplinks/1, downlink/5, downlink_error/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([update_dwell/2]).

-include("lorawan.hrl").
-include("lorawan_db.hrl").

-record(state, {gateways, recent, request_cnt, error_cnt}).
-record(gwstats, {process, target, last_gps, tx_times=[], nwk_delays=[]}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

alive(MAC, Process, Target) ->
    gen_server:cast(?MODULE, {alive, MAC, Process, Target}).

network_delay(MAC, Delay) ->
    gen_server:cast(?MODULE, {network_delay, MAC, Delay}).

report(MAC, S) ->
    gen_server:cast(?MODULE, {report, MAC, S}).

uplinks(PkList) ->
    gen_server:cast(?MODULE, {uplinks, PkList}).

downlink({MAC, GWState}, #network{gw_power=DefPower, max_eirp=MaxEIRP}, DevAddr, TxQ, PHYPayload) ->
    [#gateway{tx_rfch=RFCh, ant_gain=Gain}] = mnesia:dirty_read(gateway, MAC),
    Power = erlang:min(
        value_or_default(TxQ#txq.powe, DefPower),
        MaxEIRP-value_or_default(Gain, 0)),
    gen_server:cast(?MODULE, {downlink, {MAC, GWState}, DevAddr,
        TxQ#txq{powe=Power}, RFCh, PHYPayload}).

downlink_error(MAC, Opaque, Error) ->
    gen_server:cast(?MODULE, {downlink_error, MAC, Opaque, Error}).

value_or_default(Num, _Def) when is_number(Num) -> Num;
value_or_default(_Num, Def) -> Def.

init([]) ->
    {ok, Interval} = application:get_env(lorawan_server, server_stats_interval),
    timer:send_interval(Interval*1000, submit_stats),
    {ok, #state{gateways=dict:new(), recent=dict:new(), request_cnt=0, error_cnt=0}}.

handle_call(_Request, _From, State) ->
    {stop, {error, unknownmsg}, State}.

handle_cast({alive, MAC, Process, {Host, Port, _}=Target}, #state{gateways=Dict}=State) ->
    Stats =
        case dict:find(MAC, Dict) of
            {ok, #gwstats{process=Process, target=Target}=S} ->
                S;
            {ok, #gwstats{}=S} ->
                lorawan_utils:throw_info({gateway, MAC}, {connected, {Host, Port}}),
                S#gwstats{process=Process, target=Target};
            error ->
                lorawan_utils:throw_info({gateway, MAC}, {connected, {Host, Port}}),
                #gwstats{process=Process, target=Target}
        end,
    handle_alive(MAC, Stats),
    Dict2 = dict:store(MAC, Stats#gwstats{tx_times=[], nwk_delays=[]}, Dict),
    {noreply, State#state{gateways=Dict2}};

handle_cast({network_delay, MAC, Delay}, #state{gateways=Dict}=State) ->
    {ok, #gwstats{nwk_delays=NwkDelays}=Stats} = dict:find(MAC, Dict),
    Dict2 = dict:store(MAC, Stats#gwstats{nwk_delays=[Delay | NwkDelays]}, Dict),
    {noreply, State#state{gateways=Dict2}};

handle_cast({report, MAC, S}, State) ->
    handle_report(MAC, S),
    {noreply, State};

handle_cast({uplinks, PkList}, State) ->
    % due to reflections the gateways may have received the same frame twice
    % reflected frames received by the same gateway are ignored
    Unique = remove_duplicates(PkList, []),
    % wait for packet receptions from other gateways
    State2 =
        lists:foldl(
            fun(Frame, St) -> handle_uplink(Frame, St) end,
            State, Unique),
    {noreply, State2};

handle_cast({downlink, {MAC, GWState}, DevAddr, TxQ, RFCh, PHYPayload}, #state{gateways=Dict}=State) ->
    % lager:debug("<-- freq ~p, datr ~s, codr ~s, tmst ~p, size ~B", [TxQ#txq.freq, TxQ#txq.datr, TxQ#txq.codr, TxQ#txq.tmst, byte_size(PHYPayload)]),
    case dict:find(MAC, Dict) of
        {ok, #gwstats{process=Process, target=Target, tx_times=TxTimes}=Stats} ->
            % send data to the gateway interface handler
            gen_server:cast(Process, {send, Target, GWState, DevAddr, TxQ, RFCh, PHYPayload}),
            % store statistics
            Time = lorawan_mac_region:tx_time(byte_size(PHYPayload), TxQ),
            Dict2 = dict:store(MAC, Stats#gwstats{tx_times=[{TxQ#txq.freq, Time} | TxTimes]}, Dict),
            {noreply, State#state{gateways=Dict2}};
        error ->
            lager:warning("Downlink request ignored. Gateway ~s not connected.", [lorawan_utils:binary_to_hex(MAC)]),
            {noreply, State}
    end;

handle_cast({downlink_error, MAC, undefined, Error}, #state{error_cnt=Cnt}=State) ->
    lorawan_utils:throw_error({gateway, MAC}, Error),
    {noreply, State#state{error_cnt=Cnt+1}};
handle_cast({downlink_error, _MAC, DevAddr, Error}, #state{error_cnt=Cnt}=State) ->
    lorawan_utils:throw_error({node, DevAddr}, Error),
    {noreply, State#state{error_cnt=Cnt+1}}.

handle_info({rxq_ready, PHYPayload}, #state{recent=Recent}=State) ->
    {Gateways, Handler} = dict:fetch(PHYPayload, Recent),
    Recent2 = dict:erase(PHYPayload, Recent),
    % gateway with the best signal will be first
    Gateways2 = lists:sort(
        fun({_M1, Q1, _S1}, {_M2, Q2, _S2}) ->
            Q1#rxq.rssi >= Q2#rxq.rssi
        end,
        Gateways),
    gen_server:cast(Handler, {rxq, Gateways2}),
    {noreply, State#state{recent=Recent2}};

handle_info(submit_stats, #state{request_cnt=RequestCnt, error_cnt=ErrorCnt}=State) ->
    {atomic, ok} = mnesia:transaction(
        fun() ->
            Perf = {calendar:universal_time(), {RequestCnt, ErrorCnt}},
            Server =
                case mnesia:read(server, node(), write) of
                    [S] -> S#server{router_perf=append_perf(Perf, S#server.router_perf)};
                    [] -> #server{sname=node(), router_perf=[Perf]}
                end,
            mnesia:write(Server)
        end),
    {noreply, State#state{request_cnt=0, error_cnt=0}}.

terminate(Reason, _State) ->
    % record graceful shutdown in the log
    lager:info("gateway router terminated: ~p", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

append_perf(Perf, undefined) ->
    [Perf];
append_perf(Perf, PerfList) ->
    lists:sublist([Perf | PerfList], 50).

handle_alive(MAC, #gwstats{target=Target, last_gps=LastGPS, tx_times=TxTimes, nwk_delays=NwkDelays}) ->
    {atomic, ok} = mnesia:transaction(
        fun() ->
            case mnesia:read(gateway, MAC, write) of
                [#gateway{dwell=Dwell, delays=Delay}=G] ->
                    lorawan_admin:write(
                        G#gateway{ip_address=Target,
                            last_alive=calendar:universal_time(),
                            last_gps=LastGPS,
                            dwell=update_dwell(TxTimes, Dwell),
                            delays=append_delays(NwkDelays, Delay)});
                [] ->
                    lorawan_utils:throw_error({gateway, MAC}, unknown_mac, aggregated)
            end
        end).

update_dwell(TxTimes, undefined) ->
    update_dwell(TxTimes, []);
update_dwell(TxTimes, Dwell0) ->
    % summarize transmissions in the past hour
    Now = lorawan_utils:precise_universal_time(),
    HourAgo = lorawan_utils:apply_offset(Now, {-1,0,0}),
    Relevant = lists:filter(fun({ITime, _}) -> ITime > HourAgo end, Dwell0),
    Sum =
        lists:foldl(
            fun({_, {_, Duration, _}}, Acc) ->
                Acc + Duration
            end, 0, Relevant),
    % trim the values we don't need
    Dwell =
        if
            length(Relevant) >= 20 -> Relevant;
            true -> lists:sublist(Dwell0, 20)
        end,
    Time =
        lists:foldl(
            fun({_Freq, Time}, Acc) ->
                Acc + Time
            end, 0, TxTimes),
    if
        length(TxTimes) == 0, length(Dwell) == length(Dwell0) ->
            % nothing has changed
            Dwell;
        true ->
            % FIXME: the frequency band should be somehow considered too
            [{Now, {868, Time, Sum+Time}} | Dwell]
    end.

append_delays(NwkDelays, undefined) ->
    append_delays(NwkDelays, []);
append_delays(NwkDelays, Delay) ->
    case NwkDelays of
        List when length(List) > 0 ->
            New = {lists:min(NwkDelays), round(lists:sum(NwkDelays)/length(NwkDelays)), lists:max(NwkDelays)},
            lists:sublist([{calendar:universal_time(), New} | Delay], 50);
        _Else ->
            Delay
    end.

handle_report(MAC, S) ->
    if
        S#stat.rxfw < S#stat.rxok ->
            lorawan_utils:throw_warning({gateway, MAC}, {uplinks_lost, S#stat.rxok-S#stat.rxfw});
        true ->
            ok
    end,
    if
        S#stat.rxfw > 0, S#stat.ackr < 100 ->
            % upstream datagrams sent, but not acknowledged
            lorawan_utils:throw_warning({gateway, MAC}, {ack_lost, 100-S#stat.ackr});
        true ->
            ok
    end,
    if
        S#stat.txnb < S#stat.dwnb ->
            lorawan_utils:throw_warning({gateway, MAC}, {downlinks_lost, S#stat.dwnb-S#stat.txnb});
        true ->
            ok
    end,
    {atomic, ok} = mnesia:transaction(
        fun() ->
            case mnesia:read(gateway, MAC, write) of
                [G] ->
                    lorawan_admin:write(
                        store_pos(
                            store_desc(
                                G#gateway{last_report=calendar:universal_time()}, S), S));
                [] ->
                    lorawan_utils:throw_error({gateway, MAC}, unknown_mac, aggregated)
            end
        end).

store_pos(G, S) ->
    if
        % store gateway GPS position
        is_number(S#stat.lati), is_number(S#stat.long), S#stat.lati /= 0, S#stat.long /= 0 ->
            if
                is_number(S#stat.alti), S#stat.alti /= 0 ->
                    G#gateway{ gpspos={S#stat.lati, S#stat.long}, gpsalt=S#stat.alti };
                true ->
                    % some cheap GPS receivers give proper coordinates, but a zero altitude
                    G#gateway{ gpspos={S#stat.lati, S#stat.long} }
            end;
        % position not received
        true ->
            G
    end.

store_desc(G, S) ->
    if
        is_binary(S#stat.desc), S#stat.desc /= <<>> ->
            G#gateway{ desc=S#stat.desc };
        true ->
            G
    end.


remove_duplicates([{{MAC, RxQ, GWState}, PHYPayload} | Tail], Unique) ->
    % check if the first element is duplicate
    case lists:keytake(PHYPayload, 2, Tail) of
        {value, {{MAC2, RxQ2, GWState2}, PHYPayload}, Tail2} ->
            % select element of a better quality and re-check for other duplicates
            if
                RxQ#rxq.rssi >= RxQ2#rxq.rssi ->
                    remove_duplicates([{{MAC, RxQ, GWState}, PHYPayload} | Tail2], Unique);
                true -> % else
                    remove_duplicates([{{MAC2, RxQ2, GWState2}, PHYPayload} | Tail2], Unique)
            end;
        false ->
            remove_duplicates(Tail, [{{MAC, RxQ, GWState}, PHYPayload} | Unique])
    end;
remove_duplicates([], Unique) ->
    Unique.

handle_uplink({{MAC, RxQ, _GWState}=GWData, PHYPayload}, State) ->
    case mnesia:dirty_read(gateway, MAC) of
        [] ->
            lorawan_utils:throw_error({gateway, MAC}, unknown_mac, aggregated),
            State;
        [_Gateway] ->
            handle_uplink0({GWData, PHYPayload}, store_last_gps(MAC, RxQ, State))
    end.

handle_uplink0({GWData, PHYPayload}, #state{recent=Recent, request_cnt=Cnt}=State) ->
    case dict:find(PHYPayload, Recent) of
        error ->
            % we are not yet processing this frame
            {ok, Handler} = lorawan_handler_sup:start_child(),
            % lager:debug("--> datr ~s, codr ~s, tmst ~B, size ~B", [RxQ#rxq.datr, RxQ#rxq.codr, RxQ#rxq.tmst, byte_size(PHYPayload)]),
            gen_server:cast(Handler, {frame, GWData, PHYPayload}),
            % schedule signal quality info
            {ok, Delay} = application:get_env(lorawan_server, deduplication_delay),
            {ok, _} = timer:send_after(Delay, {rxq_ready, PHYPayload}),
            State#state{recent=dict:store(PHYPayload, {[GWData], Handler}, Recent), request_cnt=Cnt+1};
        {ok, {GWDataList, Handler}} ->
            State#state{recent=dict:store(PHYPayload, {[GWData|GWDataList], Handler}, Recent)}
    end.

store_last_gps(_MAC, #rxq{time=undefined}, State) ->
    State;
store_last_gps(MAC, #rxq{time=Time}, #state{gateways=Dict}=State) ->
    case dict:find(MAC, Dict) of
        {ok, Stats} ->
            State#state{gateways=dict:store(MAC, Stats#gwstats{last_gps=Time}, Dict)};
        error ->
            % not yet initialized
            State
    end.

% end of file
