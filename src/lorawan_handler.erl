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

-record(context, {tmst, node, profile, network}).

start_link() ->
    gen_statem:start_link(?MODULE, [], []).

init([]) ->
    {ok, idle, undefined}.

callback_mode() ->
    state_functions.

idle(cast, {frame, Gateway, <<2#000:3, _/binary>>=PHYPayload}, Data) ->
    TimeStamp = erlang:monotonic_time(milli_seconds),
    case lorawan_mac:ingest_frame(PHYPayload) of
        {join, Node, Profile, Network, AppNonce} ->
            Context = #context{tmst=TimeStamp, node=Node, profile=Profile, network=Network},
            case invoke_handler(handle_join, App, [Context]) of
                ok ->
                    {next_state, join, {Context, AppNonce}};
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
idle(cast, {frame, Gateway, PHYPayload}, Data) ->
    case lorawan_mac:ingest_frame(PHYPayload) of
        {uplink, Node, Profile, Network, Frame} ->
            Context = #context{tmst=TimeStamp, node=Node, profile=Profile, network=Network},
            case invoke_handler(handle_uplink, App, [Context,
                    #rxdata{fcnt=FCnt, port=FPort, data=RxData, last_lost=LastLost, shall_reply=ShallReply}]) of
                retransmit ->
                    % the application decided to retransmit last confirmed downlink
                    {next_state, retransmit, Frame};
                {ok, AppState} ->
                    {next_state, uplink, {Context, Frame, AppState}};
                {error, Error} ->
                    lorawan_utils:throw_error({node, Link#node.devaddr}, Error),
                    {next_state, drop, Data}
            end;
        {retransmit, Frame}
            % the server already handled this request
            {next_state, retransmit, Frame};
        {ignore, Frame}
            {next_state, log_only, Frame};
        {error, Object, Error} ->
            lorawan_utils:throw_error(Object, Error),
            {next_state, drop, Data};
        {error, Object, Error, Args} ->
            lorawan_utils:throw_error(Object, Error, Args),
            {next_state, drop, Data}
    end.

join(cast, {rxq, Gateways}, {#context{}, AppNonce}) ->
    case lorawan_mac:process_frame2(Gateways, PHYPayload) of
        ok ->
            ok;
        {send, DevAddr, TxQ, PHYPayload2} ->
            TxQ = case join_rxwin(Link) of
                0 ->
                    lager:debug("Join-Accept in RX1: ~w", [Link#node.rxwin_use]),
                    lorawan_mac_region:join1_window(Link, RxQ);
                1 ->
                    lager:debug("Join-Accept in RX2: ~w", [Link#node.rxwin_use]),
                    lorawan_mac_region:join2_window(Link, RxQ)
            end,
            {ok, PHYPayload} = encode_accept(Node, NetID, AppNonce),
            lorawan_gw_router:downlink(Req, MAC, DevAddr, TxQ, PHYPayload);
        {error, {Object, Error}} ->
            lorawan_utils:throw_error(Object, Error);
        {error, Error} ->
            lorawan_utils:throw_error(server, Error)
    end,
    % the task has been completed
    {stop, normal, State}.

join_rxwin(#node{txwin=1}) ->
    0;
join_rxwin(#node{txwin=2}) ->
    1;
join_rxwin(#node{reset_count=JoinCnt}) ->
    JoinCnt band 1.

uplink(cast, {rxq, Gateways}, {#context{}, #frame{conf=Confirm, devaddr=DevAddr, adr=ADR,
        adr_ack_req=ADRACKReq, ack=ACK, fcnt=FCnt, fport=FPort, fopts=FOpts, data=RxData}=Frame, AppState}) ->
    % process commands
    {ok, {MacConfirm, Node2, AverageQs}, FOptsOut} = lorawan_mac_commands:handle_fopts(Gateways, Node, FOpts),
    % check whether last downlink transmission was lost
    {LastLost, LostFrame} =
        case mnesia:dirty_read(pending, DevAddr) of
            [] ->
                {false, undefined};
            [#pending{confirmed=true} = Msg] when ACK == 0 ->
                lorawan_utils:throw_warning({node, DevAddr}, downlink_lost),
                {true, Msg#pending.phypayload};
            [_Msg] ->
                ok = mnesia:dirty_delete(pending, DevAddr),
                {false, undefined}
        end,
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
    % invoke applications
    case invoke_handler(handle_rxq, App, [Gateways, Node2,
            #rxdata{fcnt=FCnt, port=FPort, data=RxData, last_lost=LastLost, shall_reply=ShallReply}, AppState]) of
        retransmit ->
            lager:debug("~s retransmitting", [binary_to_hex(Link#node.devaddr)]),
            {send, Link#node.devaddr, choose_tx(Gateways, Context), LostFrame};
        {send, TxData} ->
            send_unicast(Link, choose_tx(Gateways, Context), Confirm, FOptsOut, TxData);
        ok when ShallReply ->
            % application has nothing to send, but we still need to repond
            send_unicast(Link, choose_tx(Gateways, Context), Confirm, FOptsOut, #txdata{});
        ok ->
            ok;
        {error, Error} ->
            lorawan_utils:throw_error({node, Link#node.devaddr}, Error)
    end.

choose_tx({}, _TimeStamp, #node{txwin=1}=Node) ->
    lorawan_mac_region:rx1_window(Link, RxQ);
choose_tx({}, _TimeStamp, #node{txwin=2}=Node) ->
    lorawan_mac_region:rx2_window(Link, RxQ);
choose_tx({}, TimeStamp, Node) ->
    {ok, Rx1Delay} = application:get_env(lorawan_server, rx1_delay),
    {ok, GwDelay} = application:get_env(lorawan_server, preprocessing_delay),
    % transmit as soon as possible
    case erlang:monotonic_time(milli_seconds) - TimeStamp of
        Small when Small < Rx1Delay/1000 - GwDelay ->
            lorawan_mac_region:rx1_window(Link, RxQ);
        _Big ->
            lorawan_mac_region:rx2_window(Link, RxQ)
    end.

retransmit(cast, {rxq, Gateways}, Frame) ->
    % we want to see retransmissions too
    ok = mnesia:dirty_write(rxframes, build_rxframe(Gateway, Link, RxQ, Confirm, Frame)),

    case mnesia:dirty_read(pending, Link#node.devaddr) of
        [#pending{phypayload=LostFrame}] ->
            TxQ = case Link of
                    #node{txwin=2} ->
                        lorawan_mac_region:rx2_window(Link, RxQ);
                    _Else ->
                        lorawan_mac_region:rx1_window(Link, RxQ)
                end,
            {send, Link#node.devaddr, TxQ, LostFrame};
        [] ->
            {error, nothing_to_retransmit}
    end.

drop(cast, {rxq, _Gateways}, _Data) ->
    {stop, normal, State}.

log_only(cast, {rxq, Gateways}, Frame) ->
    % log ignored frames too
    ok = mnesia:dirty_write(rxframes,
        #rxframe{frid= <<(erlang:system_time()):64>>,
            mac=Gateway#gateway.mac, rxq=RxQ, devaddr=DevAddr, fcnt=FCnt,
            confirm=bit_to_bool(Confirm), port=FPort, datetime=calendar:universal_time()}),
    {stop, normal, State}.

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

invoke_handler(Fun, App, Params) ->
    {ok, Modules} = application:get_env(lorawan_server, plugins),
    case proplists:get_value(App, Modules) of
        undefined ->
            {error, {unknown_app, App}};
        {_, Module} ->
            invoke_handler2(Module, Fun, Params);
        Module ->
            invoke_handler2(Module, Fun, Params)
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
downlink(Link, Time, TxData) ->
    TxQ = lorawan_mac_region:rx2_rf(Link#node.region, Link#node.last_rxq),
    % will ACK immediately, so server-initated Class C downlinks have ACK=0
    send_unicast(Link, TxQ#txq{time=Time}, 0, lorawan_mac_commands:build_fopts(Link), TxData).
        {send, _DevAddr, TxQ, PHYPayload2} ->
            lorawan_gw_router:downlink(#request{}, Link#node.last_mac, Link#node.devaddr, TxQ, PHYPayload2)
    end.

send_unicast(#node{devaddr=DevAddr}, TxQ, ACK, FOpts, #txdata{confirmed=Confirmed}=TxData) ->
    PHYPayload = lorawan_mac:encode_unicast(DevAddr, ACK, FOpts, TxData),
    ok = mnesia:dirty_write(pending, #pending{devaddr=DevAddr, confirmed=Confirmed, phypayload=PHYPayload}),
    {send, DevAddr, TxQ, PHYPayload};
% non #txdata received, invoke the application to perform payload encoding
send_unicast(Link, TxQ, ACK, FOpts, TxData) ->
    {FOpts2, TxData2} = lorawan_handler:encode_tx(Link, TxQ, FOpts, TxData),
    send_unicast(Link, TxQ, ACK, FOpts2, TxData2).

multicast(Group, Time, #txdata{confirmed=false} = TxData) ->
    TxQ = lorawan_mac_region:rf_fixed(Group#multicast_channel.region),
    % must be unconfirmed, ACK=0, no MAC commands allowed
    PHYPayload = lorawan_mac:encode_multicast(DevAddr, TxData),
    lorawan_gw_router:downlink(#request{}, Group#multicast_channel.mac, Group#multicast_channel.devaddr, TxQ, PHYPayload2);
multicast(_TxQ, _DevAddr, #txdata{confirmed=true}) ->
    {error, {{multicast_channel, Group#multicast_channel.devaddr}, confirmed_not_allowed}}

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
