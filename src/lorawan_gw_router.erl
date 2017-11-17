%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_gw_router).
-behaviour(gen_server).

-export([start_link/0]).
-export([alive/3, report/2, uplinks/1, downlink/5, downlink_error/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("lorawan_application.hrl").
-include("lorawan.hrl").

-record(state, {gateways, recent}).

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

alive(MAC, Process, Target) ->
    gen_server:cast({global, ?MODULE}, {alive, MAC, Process, Target}).

report(MAC, S) ->
    gen_server:cast({global, ?MODULE}, {report, MAC, S}).

uplinks(PkList) ->
    gen_server:cast({global, ?MODULE}, {uplinks, PkList}).

downlink(Req, MAC, DevAddr, TxQ, PHYPayload) ->
    [Gateway] = mnesia:dirty_read(gateways, MAC),
    Power = limit_power(Gateway, lorawan_mac_region:eirp_limits(TxQ#txq.region)),
    gen_server:cast({global, ?MODULE}, {downlink, Req, Gateway#gateway.mac, DevAddr,
        TxQ#txq{powe=Power}, Gateway#gateway.tx_rfch, PHYPayload}).

limit_power(Gateway, {EIRPdef, EIRPmax}) ->
    Power = value_or_default(Gateway#gateway.tx_powe, EIRPdef),
    Gain = value_or_default(Gateway#gateway.ant_gain, 0),
    erlang:min(Power, EIRPmax-Gain).

downlink_error(MAC, Opaque, Error) ->
    gen_server:cast({global, ?MODULE}, {downlink_error, MAC, Opaque, Error}).

value_or_default(Num, _Def) when is_number(Num) -> Num;
value_or_default(_Num, Def) -> Def.

init([]) ->
    {ok, #state{gateways=dict:new(), recent=dict:new()}}.

handle_call(_Request, _From, State) ->
    {stop, {error, unknownmsg}, State}.

handle_cast({alive, MAC, Process, {Host, Port, _}=Target}, #state{gateways=Dict}=State) ->
    case dict:find(MAC, Dict) of
        {ok, {Process, Target, Stats}} ->
            handle_alive(MAC, Stats);
        {ok, {_, _, Stats}} ->
            lorawan_utils:throw_info({gateway, MAC}, {connected, {Host, Port}}),
            handle_alive(MAC, Stats);
        error ->
            lorawan_utils:throw_info({gateway, MAC}, {connected, {Host, Port}}),
            handle_alive(MAC, [])
    end,
    Dict2 = dict:store(MAC, {Process, Target, []}, Dict),
    {noreply, State#state{gateways=Dict2}}

handle_cast({report, MAC, S}, State) ->
    handle_report(MAC, S),
    {noreply, State}.

handle_cast({uplinks, PkList}, #state{recent=Recent}=State) ->
    % due to reflections the gateways may have received the same frame twice
    % reflected frames received by the same gateway are ignored
    Unique = remove_duplicates(PkList, []),
    % wait for packet receptions from other gateways
    Recent2 =
        lists:foldl(
            fun(Frame, Dict) -> store_frame(Frame, Dict) end,
            Recent, Unique),
    {noreply, State#state{recent=Recent2}};

handle_cast({downlink, Req, MAC, DevAddr, TxQ, RFCh, PHYPayload}, #state{gateways=Dict}=State) ->
    % lager:debug("<-- datr ~s, codr ~s, tmst ~B, size ~B", [TxQ#txq.datr, TxQ#txq.codr, TxQ#txq.tmst, byte_size(PHYPayload)]),
    case dict:find(MAC, Dict) of
        {ok, {Process, Target, Stats}} ->
            % send data to the gateway interface handler
            gen_server:cast(Process, {send, Target, Req, DevAddr, TxQ, RFCh, PHYPayload}),
            % store statistics
            Time = lorawan_mac_region:tx_time(byte_size(PHYPayload), TxQ),
            Dict2 = dict:store(MAC, {Process, Target, [{TxQ#txq.freq, Time} | Stats]}, Dict),
            {noreply, State#state{gateways=Dict2}}
        error ->
            lager:warning("Downlink request ignored. Gateway ~w not connected.", [MAC]),
            {noreply, State}
    end.

handle_cast({downlink_error, MAC, undefined, Error}, State) ->
    lorawan_utils:throw_error({gateway, MAC}, Error),
    {noreply, State};
handle_cast({downlink_error, _MAC, DevAddr, Error}, State) ->
    lorawan_utils:throw_error({node, DevAddr}, Error),
    {noreply, State}.


handle_info({process, PHYPayload}, #state{recent=Recent}=State) ->
    % find the best (for now)
    [{Req, MAC, RxQ}|_Rest] = lists:sort(
        fun({_R1, _M1, Q1}, {_R2, _M2, Q2}) ->
            Q1#rxq.rssi >= Q2#rxq.rssi
        end,
        dict:fetch(PHYPayload, Recent)),
    % lager:debug("--> datr ~s, codr ~s, tmst ~B, size ~B", [RxQ#rxq.datr, RxQ#rxq.codr, RxQ#rxq.tmst, byte_size(PHYPayload)]),
    wpool:cast(handler_pool, {Req, MAC, RxQ, PHYPayload}, available_worker),
    Recent2 = dict:erase(PHYPayload, Recent),
    {noreply, State#state{recent=Recent2}}.

terminate(Reason, _State) ->
    % record graceful shutdown in the log
    lager:info("gateway router terminated: ~p", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


handle_alive(MAC, Stats) ->
    {atomic, ok} = mnesia:transaction(
        fun() ->
            case mnesia:read(gateways, MAC, write) of
                [#gateway{dwell=Dwell}=G] ->
                    mnesia:write(gateways,
                        G#gateway{last_alive=calendar:universal_time(),
                            dwell=update_dwell(Dwell, Stats)}, write);
                [] ->
                    lorawan_utils:throw_error({gateway, MAC}, unknown_mac, aggregated)
        end).

update_dwell(Dwell, undefined) ->
    Dwell;
update_dwell(undefined, Stats) ->
    update_dwell([], Stats);
update_dwell(Dwell0, Stats) ->
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
    % summarize cached transmissions
    Time =
        lists:foldl(
            fun({_Freq, Time}, Acc) ->
                Acc + Time
            end, 0, Stats),
    % FIXME: the frequency band should be somehow considered too
    [{Now, {868, Time, Sum+Time}} | Dwell].

handle_report(MAC, S) ->
    if
        S#stat.rxok < S#stat.rxnb ->
            lager:debug("Gateway ~s had ~B uplink CRC errors", [binary_to_hex(MAC), S#stat.rxnb-S#stat.rxok]);
        true ->
            ok
    end,
    if
        S#stat.rxfw < S#stat.rxok ->
            lorawan_utils:throw_warning({gateway, MAC}, {uplinks_lost, S#stat.rxok-S#stat.rxfw});
        true ->
            ok
    end,
    if
        S#stat.txnb < S#stat.dwnb ->
            lorawan_utils:throw_warning({gateway, MAC}, {downlinks_lost, S#stat.dwnb-S#stat.txnb});
        true ->
            ok
    end,
    if
        S#stat.ackr < 100 ->
            lorawan_utils:throw_warning({gateway, MAC}, {ack_lost, 100-S#stat.ackr});
        true ->
            ok
    end,
    {atomic, ok} = mnesia:transaction(
        fun() ->
            case mnesia:read(gateways, MAC, write) of
                [G] ->
                    mnesia:write(gateways,
                        store_pos(store_desc(G#gateway{last_report=calendar:universal_time()}, S), S), write);
                [] ->
                    lorawan_utils:throw_error({gateway, MAC}, unknown_mac, aggregated)
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


remove_duplicates([{Req, MAC, RxQ, PHYPayload} | Tail], Unique) ->
    % check if the first element is duplicate
    case lists:keytake(PHYPayload, 4, Tail) of
        {value, {Req2, MAC2, RxQ2, PHYPayload}, Tail2} ->
            % select element of a better quality and re-check for other duplicates
            if
                RxQ#rxq.rssi >= RxQ2#rxq.rssi ->
                    remove_duplicates([{Req, MAC, RxQ, PHYPayload} | Tail2], Unique);
                true -> % else
                    remove_duplicates([{Req2, MAC2, RxQ2, PHYPayload} | Tail2], Unique)
            end;
        false ->
            remove_duplicates(Tail, [{Req, MAC, RxQ, PHYPayload} | Unique])
    end;
remove_duplicates([], Unique) ->
    Unique.

store_frame({Req, MAC, RxQ, PHYPayload}, Dict) ->
    case dict:find(PHYPayload, Dict) of
        {ok, Frames} ->
            dict:store(PHYPayload, [{Req, MAC, RxQ}|Frames], Dict);
        error ->
            {ok, Delay} = application:get_env(lorawan_server, deduplication_delay),
            {ok, _} = timer:send_after(Delay, {process, PHYPayload}),
            dict:store(PHYPayload, [{Req, MAC, RxQ}], Dict)
    end.

% end of file
