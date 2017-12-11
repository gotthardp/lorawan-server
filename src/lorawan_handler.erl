%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_handler).
-behaviour(gen_statem).

-export([start_link/0]).
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([idle/3, join/3, uplink/3, retransmit/3, drop/3, log_only/3]).

start_link() ->
    gen_statem:start_link(?MODULE, [], []).

init([]) ->
    {ok, idle, undefined}.

callback_mode() ->
    state_functions.

idle(cast, {frame, {MAC, RxQ, _}, <<2#000:3, _/binary>>=PHYPayload}, Data) ->
    case lorawan_mac:ingest_frame(PHYPayload) of
        {join, {Network, Profile, Device}, DevAddr, DevNonce} ->
            case invoke_handler(handle_join, {Network, Profile, Device}, [{MAC, RxQ}, DevAddr]) of
                ok ->
                    {next_state, join, {{Network, Profile, Device}, DevAddr, DevNonce}};
                ignore ->
                    {next_state, drop, Data};
                {error, Error} ->
                    lorawan_utils:throw_error({node, Link#node.devaddr}, Error),
                    {next_state, drop, Data}
            end;
        ignore ->
            {next_state, drop, Data};
        {error, Object, Error} ->
            lorawan_utils:throw_error(Object, Error),
            {next_state, drop, Data};
        {error, Object, Error, Args} ->
            lorawan_utils:throw_error(Object, Error, Args),
            {next_state, drop, Data}
    end;
idle(cast, {frame, {MAC, RxQ, _}, PHYPayload}, Data) ->
    TimeStamp = erlang:monotonic_time(milli_seconds),
    case lorawan_mac:ingest_frame(PHYPayload) of
        {uplink, {Network, Profile, Node}, Frame} ->
            % check whether last downlink transmission was lost
            {LastAcked, LostFrame} =
                case mnesia:dirty_read(pending, DevAddr) of
                    [#pending{confirmed=true, phypayload=OldFrame, state=State}] when ACK == 0 ->
                        lorawan_utils:throw_warning({node, DevAddr}, downlink_lost),
                        {{lost, State}, OldFrame};
                    [#pending{confirmed=true, state=State}] when ACK == 1 ->
                        ok = mnesia:dirty_delete(pending, DevAddr),
                        {{ok, State}, undefined};
                    [_Msg] ->
                        ok = mnesia:dirty_delete(pending, DevAddr),
                        {undefined, undefined};
                    [] ->
                        {undefined, undefined}
                end,
            case invoke_handler(handle_uplink, {Network, Profile, Node}, [{MAC, RxQ}, LastAcked, Frame]) of
                {ok, AppState} ->
                    {next_state, uplink, {TimeStamp, {Network, Profile, Node}, Frame, AppState}};
                retransmit ->
                    % the application decided to retransmit last confirmed downlink
                    {next_state, retransmit, {TimeStamp, LostFrame}};
                {error, Error} ->
                    lorawan_utils:throw_error({node, Link#node.devaddr}, Error),
                    {next_state, log_only, Frame}
            end;
        {retransmit, {Network, Profile, Node}, Frame} ->
            % the server already handled this request
            case mnesia:dirty_read(pending, Node#node.devaddr) of
                [#pending{phypayload=LostFrame}] ->
                    {next_state, retransmit, {TimeStamp, LostFrame}};
                [] ->
                    lager:error("Nothing to retransmit for ~p", [Node#node.devaddr]),
                    {next_state, drop, Data}
            end;
        {ignore, Frame}
            {next_state, log_only, Frame};
        {error, Object, Error} ->
            lorawan_utils:throw_error(Object, Error),
            {next_state, drop, Data};
        {error, Object, Error, Args} ->
            lorawan_utils:throw_error(Object, Error, Args),
            {next_state, drop, Data}
    end.

join(cast, {rxq, Gateways0}, {{Network, Profile, Device}, DevAddr, DevNonce}) ->
    {MAC, RxQ, GWState} = hd(Gateways0),
    Gateways = extract_rxq(Gateways0),
    {ok, PHYPayload} = lorawan_mac:handle_accept(Gateways, Network, Device, DevAddr, DevNonce),
    TxQ = case join_rxwin(Link) of
        0 ->
            lager:debug("Join-Accept in RX1: ~w", [Link#node.rxwin_use]),
            lorawan_mac_region:join1_window(Network, RxQ);
        1 ->
            lager:debug("Join-Accept in RX2: ~w", [Link#node.rxwin_use]),
            lorawan_mac_region:join2_window(Network, RxQ)
    end,
    lorawan_gw_router:downlink({MAC, GWState}, Network, DevAddr, TxQ, PHYPayload);
    % the task has been completed
    {stop, normal, State}.

join_rxwin(#node{txwin=1}) ->
    0;
join_rxwin(#node{txwin=2}) ->
    1;
join_rxwin(#node{reset_count=JoinCnt}) ->
    JoinCnt band 1.

uplink(cast, {rxq, Gateways0}, {TimeStamp, {Network, Profile, Node},
        #frame{conf=Confirm, devaddr=DevAddr, adr=ADR, adr_ack_req=ADRACKReq, ack=ACK,
        fcnt=FCnt, fport=FPort, fopts=FOpts, data=RxData}=Frame, AppState}) ->
    {MAC, RxQ, GWState} = hd(Gateways0),
    Gateways = extract_rxq(Gateways0),
    % process commands
    {MacConfirm, Node2, AverageQs, FOptsOut} =
        lorawan_mac_commands:handle_fopts({Network, Profile, Node}, Gateways, ADR, FOpts),
    % check whether the response is required
    ShallReply = if
        Confirm == 1 ->
            % confirmed uplink received
            true;
        ADRACKReq == 1 ->
            % ADR ACK was requested
            lager:debug("ADRACKReq confirmed"),
            true;
        byte_size(FOptsOut) > 0 ->
            % have MAC commands to send
            true;
        MacConfirm == true ->
            % reception of RXParamSetupAns and RXTimingSetupAns needs to be confirmed
            true;
        true ->
            % else
            false
    end,
    mnesia:dirty_write(rxframes, build_rxframe(Gateways, Node2, Frame, AverageQs)),
    TxQ = choose_tx(Network, Node2, RxQ, TimeStamp),
    % invoke applications
    case invoke_handler(handle_rxq, {Network, Profile, Node2}, [Gateways, Frame, AppState]) of
        {send, TxData} ->
            send_unicast({MAC, GWState}, Node2, TxQ, Confirm, FOptsOut, TxData);
        ok when ShallReply ->
            % application has nothing to send, but we still need to repond
            send_unicast({MAC, GWState}, Node2, TxQ, Confirm, FOptsOut, #txdata{});
        ok ->
            ok;
        {error, Error} ->
            lorawan_utils:throw_error({node, Link#node.devaddr}, Error)
    end,
    {stop, normal, State}.

extract_rxq(Gateways) ->
    lists:map(
        fun({MAC, RxQ, _}) -> {MAC, RxQ} end,
        Gateways).

choose_tx(Network, #node{txwin=1}=Node, RxQ, _Timestamp) ->
    lorawan_mac_region:rx1_window(Network, Node, RxQ);
choose_tx(Network, #node{txwin=2}=Node, RxQ, _Timestamp) ->
    lorawan_mac_region:rx2_window(Network, RxQ);
choose_tx(Network, Node, RxQ, TimeStamp) ->
    {ok, Rx1Delay} = application:get_env(lorawan_server, rx1_delay),
    {ok, GwDelay} = application:get_env(lorawan_server, preprocessing_delay),
    % transmit as soon as possible
    case erlang:monotonic_time(milli_seconds) - TimeStamp of
        Small when Small < Rx1Delay/1000 - GwDelay ->
            lorawan_mac_region:rx1_window(Network, Link, RxQ);
        _Big ->
            lorawan_mac_region:rx2_window(Network, RxQ)
    end.

send_unicast({MAC, GWState}, #node{devaddr=DevAddr}, TxQ, ACK, FOpts, #txdata{confirmed=Confirmed}=TxData) ->
    PHYPayload = lorawan_mac:encode_unicast(DevAddr, ACK, FOpts, TxData),
    ok = mnesia:dirty_write(pending, #pending{devaddr=DevAddr, confirmed=Confirmed, phypayload=PHYPayload}),
    lorawan_gw_router:downlink({MAC, GWState}, Network, DevAddr, TxQ, PHYPayload).
% non #txdata received, invoke the application to perform payload encoding
send_unicast(Gateway, Node, TxQ, ACK, FOpts, TxData) ->
    {FOpts2, TxData2} = encode_tx(Link, TxQ, FOpts, TxData),
    send_unicast(Gateway, Node, TxQ, ACK, FOpts2, TxData2).

retransmit(cast, {rxq, Gateways}, {TimeStamp, LostFrame}) ->
    % we want to see retransmissions too
    ok = mnesia:dirty_write(rxframes, build_rxframe(Gateway, Link, RxQ, Confirm, Frame)),
    TxQ = choose_tx(Network, Node2, RxQ, TimeStamp),
    lager:debug("~s retransmitting", [binary_to_hex(Link#node.devaddr)]),
    lorawan_gw_router:downlink({MAC, GWState}, Network, Node2#node.devaddr, TxQ, LostFrame),
    {stop, normal, State}.

log_only(cast, {rxq, Gateways}, Frame) ->
    % log ignored frames too
    ok = mnesia:dirty_write(rxframes,
        #rxframe{frid= <<(erlang:system_time()):64>>,
            mac=Gateway#gateway.mac, rxq=RxQ, devaddr=DevAddr, fcnt=FCnt,
            confirm=bit_to_bool(Confirm), port=FPort, datetime=calendar:universal_time()}),
    {stop, normal, State}.

drop(cast, {rxq, _Gateways}, _Data) ->
    {stop, normal, State}.

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

invoke_handler(Fun, {_, #profile{app=App}, _}=Subject, Params) ->
    {ok, Modules} = application:get_env(lorawan_server, plugins),
    case proplists:get_value(App, Modules) of
        undefined ->
            {error, {unknown_app, App}};
        Module ->
            invoke_handler2(Module, Fun, [Subject|Params])
    end.

invoke_handler2(Module, Fun, Params) ->
    case erlang:function_exported(Module, Fun, length(Params)) of
        true ->
            apply(Module, Fun, Params);
        false ->
            lager:warning("function ~w:~w/~w not exported", [Module, Fun, length(Params)]),
            ok
    end.

% class C
downlink({Network, Profile, Node}, Time, TxData) ->
    {MAC, RxQ} = hd(Link#node.last_gateways),
    TxQ = lorawan_mac_region:rx2_rf(Link#node.region, RxQ),
    % will ACK immediately, so server-initated Class C downlinks have ACK=0
    send_unicast({MAC, undefined}, Node2, TxQ#txq{time=Time}, 0,
        lorawan_mac_commands:build_fopts({Network, Profile, Node}, []), TxData).

multicast(#multicast_channel{devaddr=DevAddr, profiles=Profiles}, Time, #txdata{confirmed=false} = TxData) ->
    % must be unconfirmed, ACK=0, no MAC commands allowed
    PHYPayload = lorawan_mac:encode_multicast(DevAddr, TxData),
    lists:foreach(
        fun(Prof) ->
            [Profile] = mnesia:dirty_read(profiles, Prof),
            multicast(DevAddr, Profile, Time, PHYPayload)
        end,
        Profiles);
multicast(_TxQ, _DevAddr, #txdata{confirmed=true}) ->
    lorawan_utils:throw_error({{multicast_channel, Group#multicast_channel.devaddr}, confirmed_not_allowed}).

multicast(DevAddr, #profile{name=Prof, network=Net}, Time, PHYPayload) ->
    [Network] = mnesia:dirty_read(networks, Net),
    TxQ = lorawan_mac_region:rf_fixed(Network#network.region),
    lists:foreach(
        fun(MAC) ->
            lorawan_gw_router:downlink({MAC, undefined}, Network, DevAddr, TxQ#txq{time=Time}, PHYPayload)
        end,
        lists:usort(
            lists:map(
                fun(Gateway) ->
                    {MAC2, _RxQ} = hd(Gateway),
                    MAC2
                end,
                mnesia:dirty_select(nodes, [{#node{profile='$1', last_gateways='$2', _='_'},
                    [{'==', '$1', Prof}], ['$2']}])
    ))).

build_rxframe(Gateway, Link, RxQ, Confirm, Frame) ->
    TXPower = case Link#node.adr_use of
        {Power, _, _} when is_integer(Power) -> Power;
        _Else -> undefined
    end,
    % #rxframe{frid, mac, rxq, average_qs, app, appid, region, devaddr, fcnt, port, data, datetime}
    #rxframe{frid= <<(erlang:system_time()):64>>,
        mac=Gateway#gateway.mac, powe=TXPower, rxq=RxQ, app=Link#node.app, appid=Link#node.appid,
        region=Link#node.region, devaddr=Link#node.devaddr, fcnt=Link#node.fcntup,
        confirm=bit_to_bool(Confirm), port=Frame#frame.fport, data=Frame#frame.data,
        datetime=calendar:universal_time()}.

% end of file
