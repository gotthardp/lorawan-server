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

idle(cast, {frame, {MAC, RxQ, _}, <<2#000:3, _/bitstring>>=PHYPayload}, Data) ->
    case lorawan_mac:ingest_frame(PHYPayload) of
        {join, {Network, Profile, Device}, DevAddr, DevNonce} ->
            case invoke_handler(handle_join, {Network, Profile, Device}, [{MAC, RxQ}, DevAddr]) of
                ok ->
                    {next_state, join, {{Network, Profile, Device}, DevAddr, DevNonce}};
                ignore ->
                    {next_state, drop, Data};
                {error, Error} ->
                    lorawan_utils:throw_error({node, DevAddr}, Error),
                    {next_state, drop, Data}
            end;
        ignore ->
            {next_state, drop, Data};
        {error, Error} ->
            lorawan_utils:throw_error(server, Error),
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
        {uplink, {Network, Profile, #node{devaddr=DevAddr}=Node}, #frame{ack=ACK}=Frame} ->
            % check whether last downlink transmission was lost
            {LastMissed, MissedDownlink} =
                case mnesia:dirty_read(pending, DevAddr) of
                    [#pending{confirmed=true, phypayload=OldFrame, receipt=Receipt}] when ACK == 0 ->
                        lorawan_utils:throw_warning({node, DevAddr}, downlink_missed),
                        {{missed, Receipt}, OldFrame};
                    [#pending{confirmed=true, receipt=Receipt}] when ACK == 1 ->
                        ok = mnesia:dirty_delete(pending, DevAddr),
                        invoke_handler(handle_delivery, {Network, Profile, Node}, [delivered, Receipt]),
                        {undefined, undefined};
                    [_Msg] ->
                        ok = mnesia:dirty_delete(pending, DevAddr),
                        {undefined, undefined};
                    [] ->
                        {undefined, undefined}
                end,
            case invoke_handler(handle_uplink, {Network, Profile, Node}, [{MAC, RxQ}, LastMissed, Frame]) of
                {ok, AppState} ->
                    {next_state, uplink, {TimeStamp, {Network, Profile, Node}, Frame, AppState}};
                retransmit ->
                    % the application decided to retransmit last confirmed downlink
                    {next_state, retransmit, {TimeStamp, {Network, Profile, Node}, Frame, MissedDownlink}};
                {error, Error} ->
                    lorawan_utils:throw_error({node, Node#node.devaddr}, Error),
                    {next_state, log_only, Frame}
            end;
        {retransmit, {Network, Profile, Node}, Frame} ->
            % the server already handled this request
            case mnesia:dirty_read(pending, Node#node.devaddr) of
                [#pending{phypayload=OldFrame}] ->
                    {next_state, retransmit, {TimeStamp, {Network, Profile, Node}, Frame, OldFrame}};
                [] ->
                    lager:error("Nothing to retransmit for ~p", [Node#node.devaddr]),
                    {next_state, drop, Data}
            end;
        {ignore, Frame} ->
            case mnesia:dirty_read(servers, node()) of
                [#server{log_ignored=true}] ->
                    {next_state, log_only, Frame};
                _Else ->
                    {next_state, drop, Data}
            end;
        {error, Error} ->
            lorawan_utils:throw_error(server, Error),
            {next_state, drop, Data};
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
    {ok, Node, PHYPayload} = lorawan_mac:handle_accept(Gateways, {Network, Profile, Device}, DevAddr, DevNonce),
    TxQ = case join_rxwin(Profile, Node) of
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

join_rxwin(#profile{txwin=1}, _Node) ->
    0;
join_rxwin(#profile{txwin=2}, _Node) ->
    1;
join_rxwin(_Profile, #node{reset_count=JoinCnt}) ->
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
    ok = mnesia:dirty_write(rxframes, build_rxframe(Gateways, {Network, Profile, Node2}, Frame)),
    TxQ = choose_tx({Network, Profile, Node2}, RxQ, TimeStamp),
    % invoke applications
    case invoke_handler(handle_rxq, {Network, Profile, Node2}, [Gateways, ShallReply, Frame, AppState]) of
        {send, TxData} ->
            send_unicast({MAC, GWState}, {Network, Profile, Node2}, TxQ, Confirm, FOptsOut, TxData);
        ok when ShallReply ->
            % application has nothing to send, but we still need to repond
            send_unicast({MAC, GWState}, {Network, Profile, Node2}, TxQ, Confirm, FOptsOut, #txdata{});
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
    {ok, PHYPayload} = lorawan_mac:encode_unicast({Network, Profile, Node}, ACK, FOpts, TxData),
    % we are about to overwrite the pending frame for this device
    case mnesia:dirty_read(pending, DevAddr) of
        [#pending{confirmed=true, receipt=Receipt}] ->
            lorawan_utils:throw_error({node, DevAddr}, downlink_expired),
            invoke_handler(handle_delivery, {Network, Profile, Node}, [lost, Receipt]);
        _Else ->
            ok
    end,
    ok = mnesia:dirty_write(pending,
        #pending{devaddr=DevAddr, confirmed=Confirmed, phypayload=PHYPayload, receipt=Receipt}),
    lorawan_gw_router:downlink({MAC, GWState}, Network, DevAddr, TxQ, PHYPayload);
% non #txdata received, invoke the application to perform payload encoding
send_unicast(Gateway, {Network, Profile, Node}, TxQ, ACK, FOpts, TxData) ->
    {FOpts2, TxData2} = invoke_handler(encode_tx, {Network, Profile, Node}, [TxQ, FOpts, TxData]),
    send_unicast(Gateway, {Network, Profile, Node}, TxQ, ACK, FOpts2, TxData2).

retransmit(cast, {rxq, Gateways0}, {TimeStamp, {Network, Profile, Node}, Frame, LostDownlink}) ->
    {MAC, RxQ, GWState} = hd(Gateways0),
    Gateways = extract_rxq(Gateways0),
    % we want to see retransmissions too
    ok = mnesia:dirty_write(rxframes, build_rxframe(Gateways, {Network, Profile, Node}, Frame)),
    TxQ = lorawan_mac_region:rx2_window(Network, Node, RxQ),
    %% FIXME: this is an emergency bugfix; we need choose RX2 for some retransmissions
    %% TxQ = choose_tx({Network, Profile, Node}, RxQ, TimeStamp),
    lager:debug("~s retransmitting", [lorawan_utils:binary_to_hex(Node#node.devaddr)]),
    lorawan_gw_router:downlink({MAC, GWState}, Network, Node#node.devaddr, TxQ, LostDownlink),
    {stop, normal, undefined}.

log_only(cast, {rxq, Gateways0}, #frame{conf=Confirm, devaddr=DevAddr, fcnt=FCnt, port=Port}) ->
    Gateways = extract_rxq(Gateways0),
    % log ignored frames too
    ok = mnesia:dirty_write(rxframes,
        #rxframe{frid= <<(erlang:system_time()):64>>, gateways=Gateways, devaddr=DevAddr,
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
downlink(#node{profile=ProfID}=Node, Time, TxData) ->
    {atomic, {ok, Network, Profile}} =
        mnesia:transaction(
            fun() ->
                lorawan_mac:load_profile(ProfID)
            end),
    {MAC, _RxQ} = hd(Node#node.gateways),
    TxQ = lorawan_mac_region:rx2_rf(Network, Node),
    % will ACK immediately, so server-initated Class C downlinks have ACK=0
    send_unicast({MAC, undefined}, {Network, Profile, Node}, TxQ#txq{time=Time}, 0,
        lorawan_mac_commands:build_fopts({Network, Profile, Node}, []), TxData).

multicast(#multicast_channel{devaddr=DevAddr, profiles=Profiles}, Time, #txdata{confirmed=false} = TxData) ->
    % must be unconfirmed, ACK=0, no MAC commands allowed
    {ok, PHYPayload} = lorawan_mac:encode_multicast(DevAddr, TxData),
    lists:foreach(
        fun(Prof) ->
            [Profile] = mnesia:dirty_read(profiles, Prof),
            multicast(DevAddr, Profile, Time, PHYPayload)
        end,
        Profiles);
multicast(#multicast_channel{devaddr=DevAddr}, _Time, #txdata{confirmed=true}) ->
    lorawan_utils:throw_error({multicast_channel, DevAddr}, confirmed_not_allowed).

multicast(DevAddr, #profile{name=Prof, network=Net}=Profile, Time, PHYPayload) ->
    [Network] = mnesia:dirty_read(networks, Net),
    TxQ = lorawan_mac_region:rx2_rf(Network, Profile),
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
                mnesia:dirty_select(nodes, [{#node{profile='$1', gateways='$2', _='_'},
                    [{'==', '$1', Prof}], ['$2']}])
    ))).

build_rxframe(Gateways, {#network{name=NetName}, #profile{app=App},
        #node{fcntup=FCnt, average_qs=AverageQs, adr_use={TXPower, _, _}}},
        #frame{conf=Confirm, devaddr=DevAddr, port=Port, data=Data}) ->
    % #rxframe{frid, gateways, average_qs, app, region, devaddr, powe, fcnt, confirm, port, data, datetime}
    #rxframe{frid= <<(erlang:system_time()):64>>, gateways=Gateways,
        average_qs=AverageQs, network=NetName, app=App, devaddr=DevAddr, powe=TXPower,
        fcnt=FCnt, confirm=bit_to_bool(Confirm), port=Port, data=Data,
        datetime=calendar:universal_time()}.

bit_to_bool(0) -> false;
bit_to_bool(1) -> true.

% end of file
