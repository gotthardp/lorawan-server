%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
% LoRaWAN 1.0.1 compliant MAC implementation
% Supports Class A devices
%
-module(lorawan_mac).

-export([process_frame/3, process_status/2]).
-export([handle_downlink/3, handle_multicast/3]).
-export([binary_to_hex/1, hex_to_binary/1]).
% for unit testing
-export([reverse/1, cipher/5, b0/4]).

-define(MAX_FCNT_GAP, 16384).

-include_lib("lorawan_server_api/include/lorawan_application.hrl").
-include("lorawan.hrl").

-record(frame, {devaddr, adr, adr_ack_req, ack, fcnt, fopts, fport, data}).

process_frame(MAC, RxQ, PHYPayload) ->
    Size = byte_size(PHYPayload)-4,
    <<Msg:Size/binary, MIC:4/binary>> = PHYPayload,
    case mnesia:dirty_read(gateways, MAC) of
        [] ->
            lorawan_utils:throw_error({gateway, MAC}, unknown_mac, aggregated);
        [Gateway] ->
            process_frame1(Gateway, RxQ, Msg, MIC)
    end.

process_status(MAC, S) ->
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
    case mnesia:dirty_read(gateways, MAC) of
        [] ->
            lorawan_utils:throw_error({gateway, MAC}, unknown_mac, aggregated);
        [G] ->
            ok = mnesia:dirty_write(gateways,
                store_status(G#gateway{last_rx=calendar:universal_time()}, S)),
            ok
    end.

store_status(G, undefined) ->
    G;
store_status(G, S) ->
    store_pos(store_desc(G, S), S).

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

process_frame1(Gateway, RxQ, <<2#000:3, _:5,
        AppEUI0:8/binary, DevEUI0:8/binary, DevNonce:2/binary>> = Msg, MIC) ->
    {AppEUI, DevEUI} = {reverse(AppEUI0), reverse(DevEUI0)},

    case mnesia:dirty_read(devices, DevEUI) of
        [] ->
            lorawan_utils:throw_error({device, DevEUI}, unknown_deveui, aggregated);
        [D] when D#device.can_join == false ->
            lager:debug("Join ignored from DevEUI ~s", [binary_to_hex(DevEUI)]),
            ok;
        [D] when D#device.appeui /= undefined, D#device.appeui /= AppEUI ->
            lorawan_utils:throw_error({device, DevEUI}, {bad_appeui, binary_to_hex(AppEUI)}, aggregated);
        [D] ->
            case aes_cmac:aes_cmac(D#device.appkey, Msg, 4) of
                MIC ->
                    handle_join(Gateway, RxQ, AppEUI, DevEUI, DevNonce, D#device.appkey);
                _MIC2 ->
                    lorawan_utils:throw_error({device, DevEUI}, bad_mic)
            end
    end;
process_frame1(Gateway, RxQ, <<MType:3, _:5,
        DevAddr0:4/binary, ADR:1, ADRACKReq:1, ACK:1, _RFU:1, FOptsLen:4,
        FCnt:16/little-unsigned-integer, FOpts:FOptsLen/binary, Body/binary>> = Msg, MIC) ->
    {FPort, FRMPayload} = case Body of
        <<>> -> {undefined, <<>>};
        <<Port:8, Payload/binary>> -> {Port, Payload}
    end,
    DevAddr = reverse(DevAddr0),
    Frame = #frame{devaddr=DevAddr, adr=ADR, adr_ack_req=ADRACKReq, ack=ACK, fcnt=FCnt, fport=FPort},
    case check_link(Gateway, DevAddr, FCnt) of
        {ok, Fresh, L} ->
            case aes_cmac:aes_cmac(L#link.nwkskey, <<(b0(MType band 1, DevAddr, L#link.fcntup, byte_size(Msg)))/binary, Msg/binary>>, 4) of
                MIC ->
                    case FPort of
                        0 when FOptsLen == 0 ->
                            Data = cipher(FRMPayload, L#link.nwkskey, MType band 1, DevAddr, L#link.fcntup),
                            handle_rxpk(Gateway, RxQ, MType, L, Fresh,
                                Frame#frame{fopts=reverse(Data), data= <<>>});
                        0 ->
                            lorawan_utils:throw_error({node, DevAddr}, double_fopts);
                        _N ->
                            Data = cipher(FRMPayload, L#link.appskey, MType band 1, DevAddr, L#link.fcntup),
                            handle_rxpk(Gateway, RxQ, MType, L, Fresh,
                                Frame#frame{fopts=FOpts, data=reverse(Data)})
                    end;
                _MIC2 ->
                    lorawan_utils:throw_error({node, DevAddr}, bad_mic)
            end;
        ignore ->
            <<Confirm:1, _:2>> = <<MType:3>>,
            ok = mnesia:dirty_write(rxframes,
                #rxframe{frid= <<(erlang:system_time()):64>>,
                    mac=Gateway#gateway.mac, rxq=RxQ, devaddr=DevAddr, fcnt=FCnt,
                    confirm=bit_to_bool(Confirm), port=FPort, datetime=calendar:universal_time()}),
            ok
    end;
process_frame1(_Gateway, _RxQ, Msg, _MIC) ->
    lager:debug("Bad frame: ~p", [Msg]),
    {error, bad_frame}.

handle_join(Gateway, RxQ, _AppEUI, DevEUI, DevNonce, AppKey) ->
    AppNonce = crypto:strong_rand_bytes(3),
    NetID = Gateway#gateway.netid,
    NwkSKey = crypto:block_encrypt(aes_ecb, AppKey,
        padded(16, <<16#01, AppNonce/binary, NetID/binary, DevNonce/binary>>)),
    AppSKey = crypto:block_encrypt(aes_ecb, AppKey,
        padded(16, <<16#02, AppNonce/binary, NetID/binary, DevNonce/binary>>)),

    {atomic, Device} = mnesia:transaction(fun() ->
        [D] = mnesia:read(devices, DevEUI, write),
        NewAddr = if
            D#device.link == undefined;
            byte_size(D#device.link) < 4 ->
                create_devaddr(NetID, Gateway#gateway.subid, 3);
            true ->
                D#device.link
        end,

        NewD = D#device{link=NewAddr, last_join=calendar:universal_time()},
        ok = mnesia:write(devices, NewD, write),
        NewD
    end),

    Link0 =
        case mnesia:dirty_read(links, Device#device.link) of
            [#link{first_reset=First, reset_count=Cnt, last_rx=undefined, devstat=Stats}]
                    when is_integer(Cnt) ->
                lorawan_utils:throw_warning({node, Device#device.link}, {repeated_reset, Cnt+1}, First),
                #link{reset_count=Cnt+1, devstat=Stats};
            [#link{devstat=Stats}] ->
                #link{first_reset=calendar:universal_time(), reset_count=0, devstat=Stats};
            [] ->
                #link{first_reset=calendar:universal_time(), reset_count=0, devstat=[]}
        end,

    lorawan_utils:throw_info({device, DevEUI}, {join, binary_to_hex(Device#device.link)}),
    Link = Link0#link{devaddr=Device#device.link, region=Device#device.region,
        app=Device#device.app, appid=Device#device.appid, appargs=Device#device.appargs,
        nwkskey=NwkSKey, appskey=AppSKey, fcntup=undefined, fcntdown=0,
        fcnt_check=Device#device.fcnt_check, txwin=Device#device.txwin,
        last_mac=Gateway#gateway.mac, last_rxq=RxQ,
        adr_flag_use=0, adr_flag_set=Device#device.adr_flag_set,
        adr_use=lorawan_mac_region:default_adr(Device#device.region),
        adr_set=Device#device.adr_set,
        rxwin_use=initial_rxwin(Device#device.rxwin_set, lorawan_mac_region:default_rxwin(Device#device.region)),
        rxwin_set=Device#device.rxwin_set, last_reset=calendar:universal_time(),
        request_devstat=Device#device.request_devstat, devstat_fcnt=undefined, last_qs=[]},
    ok = mnesia:dirty_write(links, Link),

    reset_link(Link#link.devaddr),
    case lorawan_handler:handle_join(Gateway, Device, Link) of
        ok ->
            TxQ = case join_rxwin(Link) of
                0 ->
                    lager:debug("Join-Accept in RX1: ~w", [Link#link.rxwin_use]),
                    lorawan_mac_region:join1_window(Link, RxQ);
                1 ->
                    lager:debug("Join-Accept in RX2: ~w", [Link#link.rxwin_use]),
                    lorawan_mac_region:join2_window(Link, RxQ)
            end,
            {RX1DROffset, RX2DataRate, _} = Link#link.rxwin_use,
            txaccept(TxQ, RX1DROffset, RX2DataRate, AppKey, AppNonce, NetID, Link#link.devaddr);
        {error, Error} ->
            lorawan_utils:throw_error({node, Link#link.devaddr}, Error)
    end.

join_rxwin(#link{txwin=1}) ->
    0;
join_rxwin(#link{txwin=2}) ->
    1;
join_rxwin(#link{reset_count=JoinCnt}) ->
    JoinCnt band 1.

% join response allows to send initial rx1offset and rx2dr
initial_rxwin(undefined, Default) ->
    Default;
initial_rxwin({A1, A2, _}, {B1, B2, B3}) ->
    {apply_default(A1, B1), apply_default(A2, B2), B3}.

apply_default(Value, _Default) when is_number(Value) -> Value;
apply_default(_Else, Default) -> Default.

create_devaddr(NetID, SubID, Attempts) ->
    <<_:17, NwkID:7>> = NetID,
    DevAddr =
        case SubID of
            undefined ->
                <<NwkID:7, (rand_bitstring(25))/bitstring>>;
            Bits ->
                <<NwkID:7, Bits/bitstring, (rand_bitstring(25-bit_size(Bits)))/bitstring>>
        end,
    % assert uniqueness
    case mnesia:read(links, DevAddr, read) of
        [] ->
            DevAddr;
        [#link{}] when Attempts > 0 ->
            create_devaddr(NetID, SubID, Attempts-1)
        %% FIXME: do not crash when Attempts == 0
    end.

rand_bitstring(Num) when Num rem 8 > 0 ->
    <<Bits:Num/bitstring, _/bitstring>> = crypto:strong_rand_bytes(1 + Num div 8),
    Bits;
rand_bitstring(Num) when Num rem 8 == 0 ->
    crypto:strong_rand_bytes(Num div 8).

txaccept(TxQ, RX1DROffset, RX2DataRate, AppKey, AppNonce, NetID, DevAddr) ->
    lager:debug("Join-Accept ~p, ~p, netid ~p, rx1droff ~p, rx2dr ~p, appkey ~p, appnce ~p",
        [binary_to_hex(DevAddr), TxQ, NetID, RX1DROffset, RX2DataRate, binary_to_hex(AppKey), binary_to_hex(AppNonce)]),
    MHDR = <<2#001:3, 0:3, 0:2>>,
    MACPayload = <<AppNonce/binary, NetID/binary, (reverse(DevAddr))/binary, 0:1, RX1DROffset:3, RX2DataRate:4, 1>>,
    MIC = aes_cmac:aes_cmac(AppKey, <<MHDR/binary, MACPayload/binary>>, 4),

    % yes, decrypt; see LoRaWAN specification, Section 6.2.5
    PHYPayload = crypto:block_decrypt(aes_ecb, AppKey, padded(16, <<MACPayload/binary, MIC/binary>>)),
    {send, DevAddr, TxQ, <<MHDR/binary, PHYPayload/binary>>}.


check_link(Gateway, DevAddr, FCnt) ->
    case is_ignored(DevAddr, mnesia:dirty_all_keys(ignored_links)) of
        true ->
            ignore;
        false ->
            check_link_fcnt(Gateway, DevAddr, FCnt)
    end.

is_ignored(_DevAddr, []) ->
    false;
is_ignored(DevAddr, [Key|Rest]) ->
    [#ignored_link{devaddr=MatchAddr, mask=MatchMask}] = mnesia:dirty_read(ignored_links, Key),
    case match(DevAddr, MatchAddr, MatchMask) of
        true -> true;
        false -> is_ignored(DevAddr, Rest)
    end.

match(<<DevAddr:32>>, <<MatchAddr:32>>, undefined) ->
    DevAddr == MatchAddr;
match(<<DevAddr:32>>, <<MatchAddr:32>>, <<MatchMask:32>>) ->
    (DevAddr band MatchMask) == MatchAddr.

check_link_fcnt(#gateway{netid = <<_:17, NwkID:7>>, subid=SubId}, DevAddr, FCnt) ->
    {ok, MaxLost} = application:get_env(lorawan_server, max_lost_after_reset),
    case mnesia:dirty_read(links, DevAddr) of
        [] ->
            {MyPrefix, MyPrefixSize} =
                case SubId of
                    undefined ->
                        {NwkID, 7};
                    Bits ->
                        {<<NwkID:7, Bits/bitstring>>, 7+bit_size(Bits)}
                end,
            case DevAddr of
                <<MyPrefix:MyPrefixSize/bitstring, _/bitstring>> ->
                    % report errors for devices from own network only
                    lorawan_utils:throw_error({node, DevAddr}, unknown_devaddr, aggregated);
                _Else ->
                    ok
            end,
            ignore;
        [L] when L#link.fcntup == undefined ->
            % first frame after join
            case FCnt of
                N when N == 0; N == 1 ->
                    % some device start with 0, some with 1
                    {ok, new, L#link{fcntup = N}};
                N when N < ?MAX_FCNT_GAP ->
                    lorawan_utils:throw_warning({node, DevAddr}, {uplinks_missed, N-1}),
                    {ok, new, L#link{fcntup = N}};
                _BigN ->
                    lorawan_utils:throw_error({node, DevAddr}, {fcnt_gap_too_large, FCnt}, L#link.last_rx),
                    ignore
            end;
        [L] when (L#link.fcnt_check == 2 orelse L#link.fcnt_check == 3), FCnt < L#link.fcntup, FCnt < MaxLost ->
            lager:debug("~s fcnt reset", [binary_to_hex(DevAddr)]),
            % works for 16b only since we cannot distinguish between reset and 32b rollover
            {ok, reset, L#link{fcntup = FCnt, fcntdown=0,
                adr_use=lorawan_mac_region:default_adr(L#link.region),
                rxwin_use=lorawan_mac_region:default_rxwin(L#link.region),
                last_reset=calendar:universal_time(), devstat_fcnt=undefined, last_qs=[]}};
        [L] when L#link.fcnt_check == 3 ->
            % checks disabled
            {ok, new, L#link{fcntup = FCnt}};
        [L] when FCnt == L#link.fcntup ->
            % retransmission
            {ok, retransmit, L};
        [L] when L#link.fcnt_check == 1 ->
            % strict 32-bit
            case fcnt32_gap(L#link.fcntup, FCnt) of
                1 ->
                    {ok, new, L#link{fcntup = fcnt32_inc(L#link.fcntup, 1)}};
                N when N < ?MAX_FCNT_GAP ->
                    lorawan_utils:throw_warning({node, DevAddr}, {uplinks_missed, N-1}),
                    {ok, new, L#link{fcntup = fcnt32_inc(L#link.fcntup, N)}};
                _BigN ->
                    lorawan_utils:throw_error({node, DevAddr}, {fcnt_gap_too_large, FCnt}, L#link.last_rx),
                    ignore
            end;
        [L] ->
            % strict 16-bit (default)
            case fcnt16_gap(L#link.fcntup, FCnt) of
                1 ->
                    {ok, new, L#link{fcntup = FCnt}};
                N when N < ?MAX_FCNT_GAP ->
                    lorawan_utils:throw_warning({node, DevAddr}, {uplinks_missed, N-1}),
                    {ok, new, L#link{fcntup = FCnt}};
                _BigN ->
                    lorawan_utils:throw_error({node, DevAddr}, {fcnt_gap_too_large, FCnt}, L#link.last_rx),
                    ignore
            end
    end.

fcnt16_gap(A, B) ->
    if
        A =< B -> B - A;
        A > B -> 16#FFFF - A + B
    end.

fcnt32_gap(A, B) ->
    A16 = A band 16#FFFF,
    if
        A16 > B -> 16#10000 - A16 + B;
        true  -> B - A16
    end.

fcnt32_inc(FCntUp, N) ->
    % L#link.fcntup is 32b, but the received FCnt may be 16b only
    (FCntUp + N) band 16#FFFFFFFF.

reset_link(DevAddr) ->
    ok = mnesia:dirty_delete(pending, DevAddr),
    % delete previously stored TX frames
    lorawan_db_guard:purge_txframes(DevAddr).

build_rxframe(Gateway, Link, RxQ, Confirm, Frame) ->
    TXPower = case Link#link.adr_use of
        {Power, _, _} when is_integer(Power) -> Power;
        _Else -> undefined
    end,
    % #rxframe{frid, mac, rxq, average_qs, app, appid, region, devaddr, fcnt, port, data, datetime}
    #rxframe{frid= <<(erlang:system_time()):64>>,
        mac=Gateway#gateway.mac, powe=TXPower, rxq=RxQ, app=Link#link.app, appid=Link#link.appid,
        region=Link#link.region, devaddr=Link#link.devaddr, fcnt=Link#link.fcntup,
        confirm=bit_to_bool(Confirm), port=Frame#frame.fport, data=Frame#frame.data,
        datetime=calendar:universal_time()}.

handle_rxpk(Gateway, RxQ, MType, Link, Fresh, Frame)
        when MType == 2#010; MType == 2#100 ->
    <<Confirm:1, _:2>> = <<MType:3>>,
    case Fresh of
        new ->
            handle_uplink(Gateway, RxQ, Confirm, Link, Frame);
        reset ->
            reset_link(Link#link.devaddr),
            handle_uplink(Gateway, RxQ, Confirm, Link, Frame);
        retransmit ->
            % we want to see retransmissions too
            ok = mnesia:dirty_write(rxframes, build_rxframe(Gateway, Link, RxQ, Confirm, Frame)),
            case retransmit_downlink(Link#link.devaddr) of
                {true, LostFrame} ->
                    TxQ = case Link of
                            #link{txwin=2} ->
                                lorawan_mac_region:rx2_window(Link, RxQ);
                            _Else ->
                                lorawan_mac_region:rx1_window(Link, RxQ)
                        end,
                    {send, Link#link.devaddr, TxQ, LostFrame};
                {false, _} ->
                    ok
            end
    end.

handle_uplink(Gateway, RxQ, Confirm, Link, #frame{devaddr=DevAddr, adr=ADR,
        adr_ack_req=ADRACKReq, ack=ACK, fcnt=FCnt, fport=FPort, fopts=FOpts, data=RxData}=Frame) ->
    % store parameters
    DataRate = lorawan_mac_region:datar_to_dr(Link#link.region, RxQ#rxq.datr),
    ULink =
        case Link#link.adr_use of
            {_TXPower, DataRate, _Chans} when Link#link.adr_flag_use == ADR ->
                % device didn't change any settings
                Link;
            {_TXPower, DataRate, _Chans} ->
                lager:debug("ADR indicator set to ~w", [ADR]),
                Link#link{adr_flag_use=ADR, devstat_fcnt=undefined, last_qs=[]};
            {TXPower, _OldDataRate, Chans} ->
                lager:debug("DataRate ~s switched to dr ~w", [binary_to_hex(Link#link.devaddr), DataRate]),
                Link#link{adr_flag_use=ADR, adr_use={TXPower, DataRate, Chans},
                    devstat_fcnt=undefined, last_qs=[]};
            undefined ->
                lager:debug("DataRate ~s switched to dr ~w", [binary_to_hex(Link#link.devaddr), DataRate]),
                Link#link{adr_flag_use=ADR, adr_use={undefined, DataRate, undefined},
                    devstat_fcnt=undefined, last_qs=[]}
        end,
    % process commands
    RxFrame = build_rxframe(Gateway, ULink, RxQ, Confirm, Frame),
    {ok, MacConfirm, L2, FOptsOut, RxFrame2} = lorawan_mac_commands:handle(RxQ, ULink, FOpts, RxFrame),
    ok = mnesia:dirty_write(links, L2#link{last_rx=calendar:universal_time(), last_mac=Gateway#gateway.mac, last_rxq=RxQ}),
    ok = mnesia:sync_dirty(
        fun() -> mnesia:dirty_write(rxframes, RxFrame2) end),
    % check whether last downlink transmission was lost
    {LastLost, LostFrame} = repeat_downlink(DevAddr, ACK),
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
    % invoke applications
    case lorawan_handler:handle_rx(Gateway, Link,
            #rxdata{fcnt=FCnt, port=FPort, data=RxData, last_lost=LastLost, shall_reply=ShallReply}, RxQ) of
        retransmit ->
            lager:debug("~s retransmitting", [binary_to_hex(Link#link.devaddr)]),
            {send, Link#link.devaddr, choose_tx(Link, RxQ), LostFrame};
        {send, TxData} ->
            send_unicast(Link, choose_tx(Link, RxQ), Confirm, FOptsOut, TxData);
        ok when ShallReply ->
            % application has nothing to send, but we still need to repond
            send_unicast(Link, choose_tx(Link, RxQ), Confirm, FOptsOut, #txdata{});
        ok ->
            ok;
        {error, Error} ->
            lorawan_utils:throw_error({node, Link#link.devaddr}, Error)
    end.

handle_downlink(Link, Time, TxData) ->
    TxQ = lorawan_mac_region:rx2_rf(Link#link.region, Link#link.last_rxq),
    % will ACK immediately, so server-initated Class C downlinks have ACK=0
    send_unicast(Link, TxQ#txq{time=Time}, 0, lorawan_mac_commands:build_fopts(Link), TxData).

handle_multicast(Group, Time, TxData) ->
    TxQ = lorawan_mac_region:rf_fixed(Group#multicast_group.region),
    send_multicast(TxQ#txq{time=Time}, Group#multicast_group.devaddr, TxData).

choose_tx(#link{txwin=1}=Link, RxQ) ->
    lorawan_mac_region:rx1_window(Link, RxQ);
choose_tx(#link{txwin=2}=Link, RxQ) ->
    lorawan_mac_region:rx2_window(Link, RxQ);
choose_tx(Link, RxQ) ->
    {ok, Rx1Delay} = application:get_env(lorawan_server, rx1_delay),
    {ok, GwDelay} = application:get_env(lorawan_server, preprocessing_delay),
    % transmit as soon as possible
    case erlang:monotonic_time(milli_seconds) - RxQ#rxq.srvtmst of
        Small when Small < Rx1Delay/1000 - GwDelay ->
            lorawan_mac_region:rx1_window(Link, RxQ);
        _Big ->
            lorawan_mac_region:rx2_window(Link, RxQ)
    end.

retransmit_downlink(DevAddr) ->
    case mnesia:dirty_read(pending, DevAddr) of
        [] ->
            {false, undefined};
        [Msg] ->
            {true, Msg#pending.phypayload}
    end.

repeat_downlink(DevAddr, ACK) ->
    case mnesia:dirty_read(pending, DevAddr) of
        [] ->
            {false, undefined};
        [#pending{confirmed=true} = Msg] when ACK == 0 ->
            lorawan_utils:throw_warning({node, DevAddr}, downlink_lost),
            {true, Msg#pending.phypayload};
        [_Msg] ->
            ok = mnesia:dirty_delete(pending, DevAddr),
            {false, undefined}
    end.

send_unicast(#link{devaddr=DevAddr}, TxQ, ACK, FOpts, #txdata{confirmed=false}=TxData) ->
    PHYPayload = encode_unicast(2#011, DevAddr, ACK, FOpts, TxData),
    ok = mnesia:dirty_write(pending, #pending{devaddr=DevAddr, confirmed=false, phypayload=PHYPayload}),
    {send, DevAddr, TxQ, PHYPayload};
send_unicast(#link{devaddr=DevAddr}, TxQ, ACK, FOpts, #txdata{confirmed=true}=TxData) ->
    PHYPayload = encode_unicast(2#101, DevAddr, ACK, FOpts, TxData),
    ok = mnesia:dirty_write(pending, #pending{devaddr=DevAddr, confirmed=true, phypayload=PHYPayload}),
    {send, DevAddr, TxQ, PHYPayload};
% non #txdata received, invoke the application to perform payload encoding
send_unicast(Link, TxQ, ACK, FOpts, TxData) ->
    {FOpts2, TxData2} = lorawan_handler:encode_tx(Link, TxQ, FOpts, TxData),
    send_unicast(Link, TxQ, ACK, FOpts2, TxData2).

send_multicast(TxQ, DevAddr, #txdata{confirmed=false} = TxData) ->
    % must be unconfirmed, ACK=0, no MAC commands allowed
    PHYPayload = encode_multicast(2#011, DevAddr, TxData),
    {send, DevAddr, TxQ, PHYPayload};
send_multicast(_TxQ, _DevAddr, #txdata{confirmed=true}) ->
    {error, not_allowed}.

encode_unicast(MType, DevAddr, ACK, FOpts, TxData) ->
    {atomic, L} = mnesia:transaction(
        fun() ->
            [D] = mnesia:read(links, DevAddr, write),
            FCnt = (D#link.fcntdown + 1) band 16#FFFFFFFF,
            NewD = D#link{fcntdown=FCnt},
            ok = mnesia:write(links, NewD, write),
            NewD
        end),
    encode_frame(MType, DevAddr, L#link.nwkskey, L#link.appskey,
        L#link.fcntdown, get_adr_flag(L), ACK, FOpts, TxData).

encode_multicast(MType, DevAddr, TxData) ->
    {atomic, G} = mnesia:transaction(
        fun() ->
            [D] = mnesia:read(multicast_groups, DevAddr, write),
            FCnt = (D#multicast_group.fcntdown + 1) band 16#FFFFFFFF,
            NewD = D#multicast_group{fcntdown=FCnt},
            ok = mnesia:write(multicast_groups, NewD, write),
            NewD
        end),
    encode_frame(MType, DevAddr, G#multicast_group.nwkskey, G#multicast_group.appskey,
        G#multicast_group.fcntdown, 0, 0, <<>>, TxData).

encode_frame(MType, DevAddr, NwkSKey, _AppSKey, FCnt, ADR, ACK, FOpts, #txdata{port=0, data=Data, pending=FPending}) ->
    FHDR = <<(reverse(DevAddr)):4/binary, ADR:1, 0:1, ACK:1, (bool_to_pending(FPending)):1, 0:4,
        FCnt:16/little-unsigned-integer>>,
    FRMPayload = cipher(FOpts, NwkSKey, 1, DevAddr, FCnt),
    MACPayload = <<FHDR/binary, 0:8, (reverse(FRMPayload))/binary>>,
    if
        Data == undefined; Data == <<>> -> ok;
        true -> lager:warning("Ignored application data with Port 0")
    end,
    sign_frame(MType, DevAddr, NwkSKey, FCnt, MACPayload);

encode_frame(MType, DevAddr, NwkSKey, AppSKey, FCnt, ADR, ACK, FOpts, #txdata{port=FPort, data=Data, pending=FPending}) ->
    FHDR = <<(reverse(DevAddr)):4/binary, ADR:1, 0:1, ACK:1, (bool_to_pending(FPending)):1, (byte_size(FOpts)):4,
        FCnt:16/little-unsigned-integer, FOpts/binary>>,
    MACPayload = case FPort of
        undefined when Data == undefined; Data == <<>> ->
            <<FHDR/binary>>;
        undefined ->
            lager:warning("Ignored application data without a Port number"),
            <<FHDR/binary>>;
        Num when Num > 0 ->
            FRMPayload = cipher(Data, AppSKey, 1, DevAddr, FCnt),
            <<FHDR/binary, FPort:8, (reverse(FRMPayload))/binary>>
    end,
    sign_frame(MType, DevAddr, NwkSKey, FCnt, MACPayload).

sign_frame(MType, DevAddr, NwkSKey, FCnt, MACPayload) ->
    Msg = <<MType:3, 0:3, 0:2, MACPayload/binary>>,
    MIC = aes_cmac:aes_cmac(NwkSKey, <<(b0(1, DevAddr, FCnt, byte_size(Msg)))/binary, Msg/binary>>, 4),
    <<Msg/binary, MIC/binary>>.

bool_to_pending(true) -> 1;
bool_to_pending(false) -> 0;
bool_to_pending(undefined) -> 0.

bit_to_bool(0) -> false;
bit_to_bool(1) -> true.

get_adr_flag(#link{adr_flag_set=ADR}) when ADR == undefined; ADR == 0 -> 0;
get_adr_flag(#link{adr_flag_set=ADR}) when ADR > 0 -> 1.


cipher(Bin, Key, Dir, DevAddr, FCnt) ->
    cipher(Bin, Key, Dir, DevAddr, FCnt, 1, <<>>).

cipher(<<Block:16/binary, Rest/binary>>, Key, Dir, DevAddr, FCnt, I, Acc) ->
    Si = crypto:block_encrypt(aes_ecb, Key, ai(Dir, DevAddr, FCnt, I)),
    cipher(Rest, Key, Dir, DevAddr, FCnt, I+1, <<(binxor(Block, Si, <<>>))/binary, Acc/binary>>);
cipher(<<>>, _Key, _Dir, _DevAddr, _FCnt, _I, Acc) -> Acc;
cipher(<<LastBlock/binary>>, Key, Dir, DevAddr, FCnt, I, Acc) ->
    Si = crypto:block_encrypt(aes_ecb, Key, ai(Dir, DevAddr, FCnt, I)),
    <<(binxor(LastBlock, binary:part(Si, 0, byte_size(LastBlock)), <<>>))/binary, Acc/binary>>.

ai(Dir, DevAddr, FCnt, I) ->
    <<16#01, 0,0,0,0, Dir, (reverse(DevAddr)):4/binary, FCnt:32/little-unsigned-integer, 0, I>>.

b0(Dir, DevAddr, FCnt, Len) ->
    <<16#49, 0,0,0,0, Dir, (reverse(DevAddr)):4/binary, FCnt:32/little-unsigned-integer, 0, Len>>.

binxor(<<>>, <<>>, Acc) -> Acc;
binxor(<<A, RestA/binary>>, <<B, RestB/binary>>, Acc) ->
    binxor(RestA, RestB, <<(A bxor B), Acc/binary>>).

reverse(Bin) -> reverse(Bin, <<>>).
reverse(<<>>, Acc) -> Acc;
reverse(<<H:1/binary, Rest/binary>>, Acc) ->
    reverse(Rest, <<H/binary, Acc/binary>>).

padded(Bytes, Msg) ->
    case bit_size(Msg) rem (8*Bytes) of
        0 -> Msg;
        N -> <<Msg/bitstring, 0:(8*Bytes-N)>>
    end.

% stackoverflow.com/questions/3768197/erlang-ioformatting-a-binary-to-hex
% a little magic from http://stackoverflow.com/users/2760050/himangshuj
binary_to_hex(undefined) ->
    undefined;
binary_to_hex(Id) ->
    << <<Y>> || <<X:4>> <= Id, Y <- integer_to_list(X,16)>>.

hex_to_binary(undefined) ->
    undefined;
hex_to_binary(Id) ->
    <<<<Z>> || <<X:8,Y:8>> <= Id,Z <- [binary_to_integer(<<X,Y>>,16)]>>.

% end of file
