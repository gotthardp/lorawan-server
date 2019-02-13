%
% Copyright (c) 2016-2018 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_handler).
-behaviour(gen_statem).

-export([start_link/0, downlink/3, multicast/3]).
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([idle/3, join/3, uplink/3, retransmit/3, drop/3, log_only/3]).

-include("lorawan.hrl").
-include("lorawan_db.hrl").

start_link() ->
    gen_statem:start_link(?MODULE, [], []).

init([]) ->
    {ok, idle, undefined}.

callback_mode() ->
    state_functions.

idle(cast, {frame, {MAC, RxQ, _}, <<2#000:3, _/bitstring>>=PHYPayload}, _Data) ->
    case lorawan_mac:ingest_frame(MAC, PHYPayload) of
        {join, {Network, Profile, Device}, DevAddr, DevNonce} ->
            case invoke_handler(handle_join, {Network, Profile, Device}, [{MAC, RxQ}, DevAddr]) of
                ok ->
                    {next_state, join, {{Network, Profile, Device}, DevAddr, DevNonce}};
                ignore ->
                    {next_state, drop, []};
                {error, Error} ->
                    lorawan_utils:throw_error({node, DevAddr}, Error),
                    {next_state, drop, []}
            end;
        ignore ->
            {next_state, drop, []};
        {error, Error} ->
            lorawan_utils:throw_error(server, Error),
            {next_state, drop, []};
        {error, Object, Error} ->
            lorawan_utils:throw_error(Object, Error),
            {next_state, drop, []};
        {error, Object, Error, Args} ->
            lorawan_utils:throw_error(Object, Error, Args),
            {next_state, drop, []}
    end;
idle(cast, {frame, {MAC, RxQ, _}, PHYPayload}, _Data) ->
    TimeStamp = erlang:monotonic_time(milli_seconds),
    case lorawan_mac:ingest_frame(MAC, PHYPayload) of
        {uplink, {Network, Profile, #node{devaddr=DevAddr}=Node}, #frame{ack=ACK}=Frame} ->
            % check whether last downlink transmission was lost
            LastMissed =
                case mnesia:dirty_read(pending, DevAddr) of
                    [#pending{confirmed=true, receipt=Receipt}] when ACK == 0 ->
                        lorawan_utils:throw_warning({node, DevAddr}, downlink_missed),
                        {missed, Receipt};
                    [#pending{confirmed=true, receipt=Receipt}] when ACK == 1 ->
                        ok = mnesia:dirty_delete(pending, DevAddr),
                        invoke_handler(handle_delivery, {Network, Profile, Node}, [delivered, Receipt]),
                        undefined;
                    [_Msg] ->
                        ok = mnesia:dirty_delete(pending, DevAddr),
                        undefined;
                    [] ->
                        undefined
                end,
            case invoke_handler(handle_uplink, {Network, Profile, Node}, [{MAC, RxQ}, LastMissed, Frame]) of
                {ok, AppState} ->
                    {next_state, uplink, {TimeStamp, {Network, Profile, Node}, Frame, AppState}};
                retransmit ->
                    % the application decided to retransmit last confirmed downlink
                    {next_state, retransmit, {{Network, Profile, Node}, Frame}};
                {error, Error} ->
                    lorawan_utils:throw_error({node, Node#node.devaddr}, Error),
                    {next_state, log_only, Frame}
            end;
        {retransmit, {Network, Profile, Node}, Frame} ->
            % the server already handled this request
            {next_state, retransmit, {{Network, Profile, Node}, Frame}};
        ignore ->
            {next_state, drop, []};
        {ignore, Frame} ->
            case mnesia:dirty_read(gateway, MAC) of
                [#gateway{area=AreaName}] ->
                    case mnesia:dirty_read(area, AreaName) of
                        [#area{log_ignored=true}] ->
                            {next_state, log_only, Frame};
                        _ ->
                            {next_state, drop, []}
                    end;
                _ ->
                    {next_state, drop, []}
            end;
        {error, Error} ->
            lorawan_utils:throw_error(server, Error),
            {next_state, drop, []};
        {error, Object, Error} ->
            lorawan_utils:throw_error(Object, Error),
            {next_state, drop, []};
        {error, Object, Error, Args} ->
            lorawan_utils:throw_error(Object, Error, Args),
            {next_state, drop, []}
    end.

join(cast, {rxq, Gateways0}, {{Network, Profile, Device}, DevAddr, DevNonce}) ->
    {MAC, RxQ, GWState} = hd(Gateways0),
    Gateways = extract_rxq(Gateways0),
    {ok, #node{reset_count=JoinCnt}=Node, PHYPayload} =
        lorawan_mac:handle_accept(Gateways, {Network, Profile, Device}, DevAddr, DevNonce),
    TxQ = case alternate_rxwin(Profile, JoinCnt) of
        0 ->
            lager:debug("Join-Accept in RX1: ~p", [RxQ]),
            lorawan_mac_region:join1_window(Network, RxQ);
        1 ->
            lager:debug("Join-Accept in RX2: ~p ~p", [RxQ, Node#node.rxwin_use]),
            lorawan_mac_region:join2_window(Network, Node, RxQ)
    end,
    lorawan_gw_router:downlink({MAC, GWState}, Network, DevAddr, TxQ, PHYPayload),
    % the task has been completed
    {stop, normal, undefined}.

alternate_rxwin(#profile{txwin=1}, _JoinCnt) ->
    0;
alternate_rxwin(#profile{txwin=2}, _JoinCnt) ->
    1;
alternate_rxwin(_Profile, JoinCnt) ->
    JoinCnt band 1.

uplink(cast, {rxq, Gateways0}, {TimeStamp, {Network, Profile, Node},
        #frame{conf=Confirm, adr=ADR, adr_ack_req=ADRACKReq, fopts=FOpts}=Frame, AppState}) ->
    {MAC, RxQ, GWState} = hd(Gateways0),
    Gateways = extract_rxq(Gateways0),
    % process commands
    {ok, MacConfirm, Node2, FOptsOut} =
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
    ok = mnesia:dirty_write(build_rxframe(<<"up">>, Gateways, {Network, Profile, Node2}, Frame)),
    TxQ = choose_tx({Network, Profile, Node2}, RxQ, TimeStamp),
    % invoke applications
    case invoke_handler(handle_rxq, {Network, Profile, Node2}, [Gateways, ShallReply, Frame, AppState]) of
        {send, TxData} ->
            send_unicast({MAC, GWState}, {Network, Profile, Node2}, TxQ, Confirm, FOptsOut, TxData);
        ok when ShallReply ->
            % application has nothing to send, but we still need to repond
            send_unicast({MAC, GWState}, {Network, Profile, Node2}, TxQ, Confirm, FOptsOut, #txdata{data= <<>>});
        ok ->
            ok;
        {error, Error} ->
            lorawan_utils:throw_error({node, Node#node.devaddr}, Error)
    end,
    {stop, normal, undefined}.

extract_rxq(Gateways) ->
    lists:map(
        fun({MAC, RxQ, _}) -> {MAC, RxQ} end,
        Gateways).

choose_tx({Network, #profile{txwin=1}, Node}, RxQ, _Timestamp) ->
    lorawan_mac_region:rx1_window(Network, Node, RxQ);
choose_tx({Network, #profile{txwin=2}, Node}, RxQ, _Timestamp) ->
    lorawan_mac_region:rx2_window(Network, Node, RxQ);
choose_tx({#network{rx1_delay=Rx1Delay}=Network, _Profile, Node}, RxQ, TimeStamp) ->
    {ok, GwDelay} = application:get_env(lorawan_server, gateway_delay),
    % transmit as soon as possible
    case erlang:monotonic_time(milli_seconds) - TimeStamp of
        Small when Small < Rx1Delay*1000 - GwDelay ->
            lorawan_mac_region:rx1_window(Network, Node, RxQ);
        _Big ->
            lorawan_mac_region:rx2_window(Network, Node, RxQ)
    end.

send_unicast({MAC, GWState}, {Network, Profile, #node{devaddr=DevAddr}=Node}, TxQ, ACK, FOpts,
        #txdata{confirmed=Confirmed, receipt=Receipt}=TxData) ->
    {ok, Node2, PHYPayload} = lorawan_mac:encode_unicast({Network, Profile, Node}, ACK, FOpts, TxData),
    % we are about to overwrite the pending frame for this device
    case mnesia:dirty_read(pending, DevAddr) of
        [#pending{confirmed=true, receipt=Receipt}] ->
            lorawan_utils:throw_error({node, DevAddr}, downlink_expired),
            invoke_handler(handle_delivery, {Network, Profile, Node2}, [lost, Receipt]);
        _Else ->
            ok
    end,
    ok = mnesia:dirty_write(build_rxframe(<<"down">>, MAC, {Network, Profile, Node2}, TxData)),
    ok = mnesia:dirty_write(pending,
        #pending{devaddr=DevAddr, confirmed=Confirmed, phypayload=PHYPayload,
            sent_count=1, receipt=Receipt}),
    lorawan_gw_router:downlink({MAC, GWState}, Network, DevAddr, TxQ, PHYPayload);
% non #txdata received, invoke the application to perform payload encoding
send_unicast(Gateway, {Network, Profile, Node}, TxQ, ACK, FOpts, TxData) ->
    {FOpts2, TxData2} = invoke_handler(encode_tx, {Network, Profile, Node}, [TxQ, FOpts, TxData]),
    send_unicast(Gateway, {Network, Profile, Node}, TxQ, ACK, FOpts2, TxData2).

retransmit(cast, {rxq, Gateways0}, {{Network, Profile, #node{devaddr=DevAddr}=Node}, Frame}) ->
    {atomic, ToTransmit} =
        mnesia:transaction(fun() ->
            case mnesia:read(pending, DevAddr, write) of
                [#pending{phypayload=Payload, sent_count=Cnt}=Pen] ->
                    ok = mnesia:write(Pen#pending{sent_count=Cnt+1}),
                    % first retransmission is sent_count=2, so the RX1 will be used
                    {Payload, Cnt+1};
                [] ->
                    undefined
            end
        end),
    case ToTransmit of
        {PHYPayload, Count} ->
            {MAC, RxQ, GWState} = hd(Gateways0),
            Gateways = extract_rxq(Gateways0),
            % we want to see retransmissions too
            ok = mnesia:dirty_write(build_rxframe(<<"re-up">>, Gateways, {Network, Profile, Node}, Frame)),
            TxQ =
                case alternate_rxwin(Profile, Count) of
                    0 -> lorawan_mac_region:rx1_window(Network, Node, RxQ);
                    1 -> lorawan_mac_region:rx2_window(Network, Node, RxQ)
                end,
            lager:debug("~s retransmitting ~B", [lorawan_utils:binary_to_hex(DevAddr), Count]),
            lorawan_gw_router:downlink({MAC, GWState}, Network, DevAddr, TxQ, PHYPayload),
            {stop, normal, []};
        undefined ->
            lager:error("Nothing to retransmit for ~p", [DevAddr]),
            {next_state, drop, []}
    end.

log_only(cast, {rxq, Gateways0}, #frame{conf=Confirm, devaddr=DevAddr, fcnt=FCnt, port=Port}) ->
    Gateways = extract_rxq(Gateways0),
    {ok, FrId} = eid:get_bin(),
    % log ignored frames too
    ok = mnesia:dirty_write(
        #rxframe{frid=FrId, gateways=Gateways, devaddr=DevAddr,
            fcnt=FCnt, confirm=bit_to_bool(Confirm), port=Port,
            datetime=calendar:universal_time()}),
    {stop, normal, undefined}.

drop(cast, {rxq, _Gateways}, _Data) ->
    {stop, normal, undefined}.

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

invoke_handler(Fun, {_, #profile{app=App}, _}=Subject, Params) ->
    {ok, Modules} = application:get_env(lorawan_server, applications),
    case proplists:get_value(App, Modules) of
        undefined ->
            % if it's not internal, then it must be external
            apply(lorawan_application_backend, Fun, [Subject|Params]);
        {_, Module} ->
            invoke_handler2(Module, Fun, [Subject|Params]);
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
downlink({Network, Profile, #node{gateways=[{MAC, _RxQ}|_]}=Node}, Time, TxData) ->
    TxQ = lorawan_mac_region:rx2_rf(Network, Node),
    % will ACK immediately, so server-initated Class C downlinks have ACK=0
    send_unicast({MAC, undefined}, {Network, Profile, Node}, TxQ#txq{time=Time}, 0,
        lorawan_mac_commands:build_fopts({Network, Profile, Node}, []), TxData);
downlink({_, _, #node{devaddr=DevAddr}}, _, _) ->
    lorawan_utils:throw_error({node, DevAddr}, no_uplink_yet).

multicast(#multicast_channel{devaddr=DevAddr, profiles=Profiles}=Channel, Time, #txdata{confirmed=false} = TxData) ->
    % must be unconfirmed, ACK=0, no MAC commands allowed
    {ok, _, PHYPayload} = lorawan_mac:encode_multicast(DevAddr, TxData),
    lists:foreach(
        fun(ProfName) ->
            [#profile{group=GroupName}=Profile] = mnesia:dirty_read(profile, ProfName),
            [#group{network=NetName}] = mnesia:dirty_read(group, GroupName),
            [Network] = mnesia:dirty_read(network, NetName),
            Gateways = mac_for_profile(ProfName),
            ok = mnesia:dirty_write(build_rxframe(<<"bcast">>, Gateways, {Network, Profile, Channel}, TxData)),
            TxQ = lorawan_mac_region:rx2_rf(Network, Profile),
            lists:foreach(
                fun(MAC) ->
                    lorawan_gw_router:downlink({MAC, undefined},
                        Network, DevAddr, TxQ#txq{time=Time}, PHYPayload)
                end,
                Gateways)
        end,
        Profiles);
multicast(#multicast_channel{devaddr=DevAddr}, _Time, #txdata{confirmed=true}) ->
    lorawan_utils:throw_error({multicast_channel, DevAddr}, confirmed_not_allowed).

mac_for_profile(ProfName) ->
    lists:usort(
        lists:filtermap(
            fun
                ([{MAC, _RxQ} | _]) -> {true, MAC};
                (_Else) -> false
            end,
            mnesia:dirty_select(node, [{#node{profile='$1', gateways='$2', _='_'},
                [{'==', '$1', ProfName}], ['$2']}]))).

build_rxframe(Dir, Gateways, {#network{name=NetName}, #profile{app=App},
        #node{location=Location, fcntup=FCnt, average_qs=AverageQs, adr_use={TXPower, _, _}}},
        #frame{conf=Confirm, devaddr=DevAddr, port=Port, data=Data}) ->
    {ok, FrId} = eid:get_bin(),
    % #rxframe{frid, dir, network, app, devaddr, appargs, gateways, average_qs, powe, fcnt, confirm, port, data, datetime}
    #rxframe{frid=FrId, dir=Dir, network=NetName,
        app=App, devaddr=DevAddr, location=Location, gateways=Gateways,
        average_qs=AverageQs, powe=TXPower,
        fcnt=FCnt, confirm=bit_to_bool(Confirm), port=Port, data=Data,
        datetime=lorawan_utils:precise_universal_time()};
build_rxframe(Dir, MAC, {#network{name=NetName}, #profile{app=App},
        #node{location=Location, devaddr=DevAddr, fcntdown=FCnt}},
        #txdata{confirmed=Confirm, port=Port, data=Data}) ->
    {ok, FrId} = eid:get_bin(),
    #rxframe{frid=FrId, dir=Dir, network=NetName,
        app=App, devaddr=DevAddr, location=Location, gateways=[{MAC, #rxq{}}],
        fcnt=FCnt, confirm=Confirm, port=Port, data=Data,
        datetime=lorawan_utils:precise_universal_time()};
build_rxframe(Dir, MAC, {#network{name=NetName}, #profile{app=App},
        #multicast_channel{devaddr=DevAddr, fcntdown=FCnt}},
        #txdata{confirmed=Confirm, port=Port, data=Data}) ->
    {ok, FrId} = eid:get_bin(),
    #rxframe{frid=FrId, dir=Dir, network=NetName,
        app=App, devaddr=DevAddr, gateways=[{M, #rxq{}} || M <- MAC],
        fcnt=FCnt, confirm=Confirm, port=Port, data=Data,
        datetime=lorawan_utils:precise_universal_time()}.

bit_to_bool(0) -> false;
bit_to_bool(1) -> true.

% end of file
