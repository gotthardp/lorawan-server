%
% Copyright (c) 2016-2018 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_mac_commands).

-export([handle_fopts/4, build_fopts/2, merge_rxwin/2]).

-include("lorawan_db.hrl").

handle_fopts({Network, Profile, Node}, Gateways, ADR, FOpts) ->
    FOptsIn = parse_fopts(FOpts),
    case FOptsIn of
        [] -> ok;
        List1 -> lager:debug("~s -> ~w", [lorawan_utils:binary_to_hex(Node#node.devaddr), List1])
    end,
    % process incoming responses
    {atomic, {MacConfirm, Node2}} =
        mnesia:transaction(
        fun() ->
            [N0] = mnesia:read(node, Node#node.devaddr, write),
            {MC, N2} = handle_fopts0(
                {Network, Profile, store_actual_adr(Gateways, ADR, Network, N0)},
                Gateways, FOptsIn),
            ok = lorawan_admin:write(N2),
            {MC, N2}
        end),
    % process requests
    FOptsOut =
        lists:foldl(
            fun (link_check_req, Acc) -> [send_link_check(Gateways) | Acc];
                (device_time_req, Acc) -> [send_device_time(Gateways) | Acc];
                (_Else, Acc) -> Acc
            end,
            [], FOptsIn),
    % check for new requests
    {ok, MacConfirm, Node2, build_fopts({Network, Profile, Node2}, FOptsOut)}.

handle_fopts0({Network, Profile, Node0}, Gateways, FOptsIn) ->
    {MacConfirm, Node1} = handle_rxwin(FOptsIn, Network, Profile,
        handle_adr(FOptsIn,
        handle_status(FOptsIn, Network, Node0))),
    {ok, FramesRequired} = application:get_env(lorawan_server, frames_before_adr),
    % maintain quality statistics
    {_, RxQ} = hd(Gateways),
    {LastQs, AverageQs} = append_qs({RxQ#rxq.rssi, RxQ#rxq.lsnr}, Node1#node.last_qs, FramesRequired),
    Node2 = auto_adr(Network, Profile, Node1#node{last_qs=LastQs, average_qs=AverageQs}),
    {MacConfirm,
        Node2#node{last_rx=calendar:universal_time(), gateways=Gateways}}.

append_qs(SNR, undefined, _Required) ->
    {[SNR], undefined};
append_qs(SNR, LastQs, Required) when length(LastQs) < Required ->
    {[SNR | LastQs], undefined};
append_qs(SNR, LastQs, Required) ->
    LastQs2 = lists:sublist([SNR | LastQs], Required),
    AverageQs = average_qs(lists:unzip(LastQs2)),
    {LastQs2, AverageQs}.

average_qs({List1, List2}) ->
    {average_qs0(List1), average_qs0(List2)}.

average_qs0(List) ->
    Avg = lists:sum(List)/length(List),
    Sigma = math:sqrt(lists:sum([(N-Avg)*(N-Avg) || N <- List])/length(List)),
    Avg-Sigma.

build_fopts({Network, Profile, Node}, FOptsOut0) ->
    FOptsOut = send_adr(Network, Node,
        set_rxwin(Profile, Node,
        request_status(Profile, Node, FOptsOut0))),
    case FOptsOut of
        [] -> ok;
        List2 -> lager:debug("~s <- ~w", [lorawan_utils:binary_to_hex(Node#node.devaddr), List2])
    end,
    encode_fopts(FOptsOut).


parse_fopts(<<16#02, Rest/binary>>) ->
    [link_check_req | parse_fopts(Rest)];
parse_fopts(<<16#03, _RFU:5, PowerACK:1, DataRateACK:1, ChannelMaskACK:1, Rest/binary>>) ->
    [{link_adr_ans, PowerACK, DataRateACK, ChannelMaskACK} | parse_fopts(Rest)];
parse_fopts(<<16#04, Rest/binary>>) ->
    [duty_cycle_ans | parse_fopts(Rest)];
parse_fopts(<<16#05, _RFU:5, RX1DROffsetACK:1, RX2DataRateACK:1, ChannelACK:1, Rest/binary>>) ->
    [{rx_param_setup_ans, RX1DROffsetACK, RX2DataRateACK, ChannelACK} | parse_fopts(Rest)];
parse_fopts(<<16#06, Battery:8, _RFU:2, Margin:6/signed, Rest/binary>>) ->
    [{dev_status_ans, Battery, Margin} | parse_fopts(Rest)];
parse_fopts(<<16#07, _RFU:6, DataRateRangeOK:1, ChannelFreqOK:1, Rest/binary>>) ->
    [{new_channel_ans, DataRateRangeOK, ChannelFreqOK} | parse_fopts(Rest)];
parse_fopts(<<16#08, Rest/binary>>) ->
    [rx_timing_setup_ans | parse_fopts(Rest)];
parse_fopts(<<16#09, Rest/binary>>) ->
    [tx_param_setup_ans | parse_fopts(Rest)];
parse_fopts(<<16#0A, _RFU:6, UplinkFreqExists:1, ChannelFreqOK:1, Rest/binary>>) ->
    [{di_channel_ans, UplinkFreqExists, ChannelFreqOK} | parse_fopts(Rest)];
parse_fopts(<<16#0D, Rest/binary>>) ->
    [device_time_req | parse_fopts(Rest)];
parse_fopts(<<>>) ->
    [];
parse_fopts(Unknown) ->
    lager:warning("Unknown command ~p", [lorawan_utils:binary_to_hex(Unknown)]),
    [].

encode_fopts([{link_check_ans, Margin, GwCnt} | Rest]) ->
    <<16#02, Margin, GwCnt, (encode_fopts(Rest))/binary>>;
encode_fopts([{link_adr_req, DataRate, TXPower, ChMask, ChMaskCntl, NbRep} | Rest]) ->
    <<16#03, DataRate:4, TXPower:4, ChMask:16/little-unsigned-integer, 0:1, ChMaskCntl:3, NbRep:4, (encode_fopts(Rest))/binary>>;
encode_fopts([{duty_cycle_req, MaxDCycle} | Rest]) ->
    <<16#04, 0:4, MaxDCycle:4, (encode_fopts(Rest))/binary>>;
encode_fopts([{rx_param_setup_req, RX1DROffset, RX2DataRate, Frequency} | Rest]) ->
    <<16#05, 0:1, RX1DROffset:3, RX2DataRate:4, Frequency:24/little-unsigned-integer, (encode_fopts(Rest))/binary>>;
encode_fopts([dev_status_req | Rest]) ->
    <<16#06, (encode_fopts(Rest))/binary>>;
encode_fopts([{new_channel_req, ChIndex, Freq, MaxDR, MinDR} | Rest]) ->
    <<16#07, ChIndex, Freq:24/little-unsigned-integer, MaxDR:4, MinDR:4, (encode_fopts(Rest))/binary>>;
encode_fopts([{rx_timing_setup_req, Delay} | Rest]) ->
    <<16#08, 0:4, Delay:4, (encode_fopts(Rest))/binary>>;
encode_fopts([{tx_param_setup_req, DownDwell, UplinkDwell, MaxEIRP} | Rest]) ->
    <<16#09, 0:2, DownDwell:1, UplinkDwell:1, MaxEIRP:4, (encode_fopts(Rest))/binary>>;
encode_fopts([{di_channel_req, ChIndex, Freq} | Rest]) ->
    <<16#0A, ChIndex, Freq:24/little-unsigned-integer, (encode_fopts(Rest))/binary>>;
encode_fopts([{device_time_ans, MsSinceEpoch} | Rest]) ->
    Ms = trunc((MsSinceEpoch rem 1000) / 3.90625), % 0.5^8
    <<16#0D, (MsSinceEpoch div 1000):32/little-unsigned-integer, Ms, (encode_fopts(Rest))/binary>>;
encode_fopts([]) ->
    <<>>.


store_actual_adr([{_MAC, RxQ}|_], ADR, #network{region=Region, init_chans=InitChans, max_power=MaxPower}, Node) ->
    % store parameters
    DataRate = lorawan_mac_region:datar_to_dr(Region, RxQ#rxq.datr),
    case Node#node.adr_use of
        {TXPower, DataRate, Chans}
                when is_number(TXPower), is_list(Chans), Node#node.adr_flag == ADR ->
            % device didn't change any settings
            Node;
        {TXPower, DataRate, Chans}
                when is_number(TXPower), is_list(Chans) ->
            lager:debug("ADR indicator set to ~w", [ADR]),
            Node#node{adr_flag=ADR, devstat_fcnt=undefined, last_qs=[]};
        {TXPower, _OldDataRate, Chans}
                when is_number(TXPower), is_list(Chans) ->
            lager:debug("DataRate ~s switched to dr ~w", [lorawan_utils:binary_to_hex(Node#node.devaddr), DataRate]),
            Node#node{adr_flag=ADR, adr_use={TXPower, DataRate, Chans},
                devstat_fcnt=undefined, last_qs=[]};
        _Else ->
            % this should not happen
            lager:debug("DataRate ~s initialized to dr ~w", [lorawan_utils:binary_to_hex(Node#node.devaddr), DataRate]),
            Node#node{adr_flag=ADR, adr_use={MaxPower, DataRate, InitChans},
                devstat_fcnt=undefined, last_qs=[]}
    end.

handle_adr(FOptsIn, Node) ->
    case find_adr(FOptsIn) of
        {1, 1, 1} ->
            case merge_adr(Node#node.adr_set, Node#node.adr_use) of
                Unchanged when Unchanged == Node#node.adr_use ->
                    lager:debug("LinkADRReq ~s succeeded (enforcement only)",
                        [lorawan_utils:binary_to_hex(Node#node.devaddr)]),
                    % the desired ADR is already used
                    Node#node{adr_set=undefined, adr_failed=[]};
                NodeSet ->
                    lager:debug("LinkADRReq ~s succeeded",
                        [lorawan_utils:binary_to_hex(Node#node.devaddr)]),
                    Node#node{adr_set=undefined, adr_use=NodeSet, adr_failed=[], devstat_fcnt=undefined, last_qs=[]}
            end;
        {PowerACK, DataRateACK, ChannelMaskACK} ->
            lorawan_utils:throw_warning({node, Node#node.devaddr},
                {adr_req_failed, {PowerACK, DataRateACK, ChannelMaskACK}}),
            % indicate the settings that failed
            Node#node{adr_failed = add_when_zero(<<"power">>, PowerACK,
                add_when_zero(<<"data_rate">>, DataRateACK, add_when_zero(<<"channel_mask">>, ChannelMaskACK, [])))};
        undefined ->
            Node
    end.

find_adr(FOptsIn) ->
    lists:foldr(
        fun ({link_adr_ans, Power, DataRate, ChannelMask}, undefined) ->
                {Power, DataRate, ChannelMask};
            ({link_adr_ans, _Power, _DataRate, ChannelMask}, {LastPower, LastDataRate, LastChannelMask}) ->
                % all ChannelMasks must be accepted
                % the device processes the DataRate, TXPower and NbTrans from the last message only
                {LastPower, LastDataRate, ChannelMask band LastChannelMask};
            (_Else, Last) -> Last
        end,
        undefined, FOptsIn).

handle_rxwin(FOptsIn, _Network, Profile, Node) ->
    case find_rxwin(FOptsIn) of
        {1, 1, 1} ->
            if
                Profile#profile.rxwin_set == Node#node.rxwin_use ->
                    lager:debug("RXParamSetupAns ~s succeeded (enforcement only)", [lorawan_utils:binary_to_hex(Node#node.devaddr)]),
                    {true, Node#node{rxwin_failed=[]}};
                true ->
                    lager:debug("RXParamSetupAns ~s succeeded", [lorawan_utils:binary_to_hex(Node#node.devaddr)]),
                    {true, Node#node{rxwin_use=Profile#profile.rxwin_set, rxwin_failed=[]}}
            end;
        {RX1DROffsetACK, RX2DataRateACK, ChannelACK} ->
            lorawan_utils:throw_warning({node, Node#node.devaddr}, {rxwin_setup_failed, {RX1DROffsetACK, RX2DataRateACK, ChannelACK}}),
            % indicate the settings that failed
            {true, Node#node{rxwin_failed = add_when_zero(<<"dr_offset">>, RX1DROffsetACK,
                add_when_zero(<<"rx2_data_rate">>, RX2DataRateACK, add_when_zero(<<"channel">>, ChannelACK, [])))}};
        undefined ->
            {false, Node}
    end.

find_rxwin(FOptsIn) ->
    lists:foldl(
        fun ({rx_param_setup_ans, RX1DROffsetACK, RX2DataRateACK, ChannelACK}, _Prev) -> {RX1DROffsetACK, RX2DataRateACK, ChannelACK};
            (_Else, Last) -> Last
        end,
        undefined, FOptsIn).

handle_status(FOptsIn, #network{region=Region}, Node) ->
    case find_status(FOptsIn) of
        {Battery, Margin} ->
            % compute a maximal D/L SNR
            {_, DataRate, _} = Node#node.adr_use,
            {OffUse, _, _} = Node#node.rxwin_use,
            MaxSNR = lorawan_mac_region:max_downlink_snr(Region, DataRate, OffUse),
            lager:debug("DevStatus: battery ~B, margin: ~B (max ~.1f)", [Battery, Margin, MaxSNR]),
            Node#node{devstat_time=calendar:universal_time(), devstat_fcnt=Node#node.fcntup,
                devstat=append_status({calendar:universal_time(), Battery, Margin, MaxSNR}, Node#node.devstat)};
        undefined ->
            Node
    end.

append_status(Status, undefined) ->
    [Status];
% backward compatibility
append_status(Status, {Battery, Margin}) ->
    [Status, {calendar:universal_time(), Battery, Margin}];
append_status(Status, List) ->
    lists:sublist([Status | List], 50).

find_status(FOptsIn) ->
    lists:foldl(
        fun ({dev_status_ans, Battery, Margin}, _Prev) -> {Battery, Margin};
            (_Else, Last) -> Last
        end,
        undefined, FOptsIn).

send_link_check([{_MAC, RxQ}|_]=Gateways) ->
    #rxq{datr=DataRate, lsnr=SNR} = RxQ,
    Margin = trunc(SNR - lorawan_mac_region:max_uplink_snr(DataRate)),
    lager:debug("LinkCheckAns: margin: ~B, gateways: ~B", [Margin, length(Gateways)]),
    {link_check_ans, Margin, length(Gateways)}.

send_device_time([{_MAC, #rxq{time=undefined}}|_]) ->
    MsSinceEpoch = time_to_gps(lorawan_utils:precise_universal_time()),
    lager:debug("DeviceTimeAns: time: ~B (from local)", [MsSinceEpoch]),
    % no time provided by the gateway, we do our best
    {device_time_ans, MsSinceEpoch};
send_device_time([{_MAC, #rxq{time=Time, tmms=undefined}}|_]) ->
    MsSinceEpoch = time_to_gps(Time),
    lager:debug("DeviceTimeAns: time: ~B (from gateway)", [MsSinceEpoch]),
    % we got GPS time, but not milliseconds
    {device_time_ans, MsSinceEpoch};
send_device_time([{_MAC, #rxq{tmms=MsSinceEpoch}}|_]) ->
    lager:debug("DeviceTimeAns: time: ~B", [MsSinceEpoch]),
    % this is the easiest
    {device_time_ans, MsSinceEpoch}.

time_to_gps({Date, {Hours, Min, Secs}}) ->
    TotalSecs = calendar:datetime_to_gregorian_seconds({Date, {Hours, Min, trunc(Secs)}})
            - calendar:datetime_to_gregorian_seconds({{1980, 1, 6}, {0, 0, 0}})
            + 17, % leap seconds
    1000*TotalSecs + (Secs - trunc(Secs)).


auto_adr(Network, #profile{adr_mode=1}=Profile, #node{adr_flag=1, adr_failed=Failed}=Node)
        when Failed == undefined; Failed == [] ->
    case merge_adr(Node#node.adr_set, Node#node.adr_use) of
        Unchanged when Unchanged == Node#node.adr_use, is_tuple(Node#node.average_qs) ->
            % have enough data and no other change was requested
            calculate_adr(Network, Profile, Node);
        _Else ->
            Node
    end;
auto_adr(_Network, #profile{adr_mode=2}=Profile, #node{adr_flag=1, adr_failed=Failed}=Node)
        when Failed == undefined; Failed == [] ->
    case merge_adr(Profile#profile.adr_set, Node#node.adr_use) of
        Unchanged when Unchanged == Node#node.adr_use ->
            % no change to profile settings
            Node;
        _Else ->
            % use the profile-defined value
            Node#node{adr_set = Profile#profile.adr_set}
    end;
auto_adr(_Network, _Profile, Node) ->
    % ADR is Disabled (or undefined)
    Node.

calculate_adr(#network{region=Region, max_datr=NwkMaxDR1, max_power=MaxPower,
        min_power=MinPower, cflist=CFList}, #profile{max_datr=MaxDRLimit},
        #node{average_qs={AvgRSSI, AvgSNR}, adr_use={TxPower, DataRate, Chans}}=Node) ->
    % maximum DR supported by some channel
    NwkMaxDR2 =
        lists:foldl(
            fun
                ({_,_,Max}, Acc) when is_integer(Max) -> max(Max, Acc);
                (_, Acc) -> Acc
            end,
            NwkMaxDR1,
            if
                is_list(CFList) -> CFList;
                true -> []
            end),
    % apply device limit for maximum DR
    MaxDR =
        if
            MaxDRLimit == undefined -> NwkMaxDR2;
            true -> min(NwkMaxDR2, MaxDRLimit)
        end,
    % how many SF steps (per Table 13) are between current SNR and current sensitivity?
    % there is 2.5 dB between the DR, so divide by 3 to get more margin
    MaxSNR = lorawan_mac_region:max_uplink_snr(Region, DataRate)+10,
    StepsDR = trunc((AvgSNR-MaxSNR)/3),
    DataRate2 = if
            StepsDR > 0, DataRate < MaxDR ->
                lager:debug("DataRate ~s: average snr ~w ~w = ~w, dr ~w -> step ~w",
                    [lorawan_utils:binary_to_hex(Node#node.devaddr), round(AvgSNR), MaxSNR, round(AvgSNR-MaxSNR), DataRate, StepsDR]),
                min(DataRate+StepsDR, MaxDR);
            true ->
                DataRate
        end,
    % receiver sensitivity for maximal DR in all regions is -120 dBm, we try to stay at -100 dBm
    TxPower2 = if
            AvgRSSI > -96, TxPower < MinPower ->
                PwrStepUp = trunc((AvgRSSI+100)/4), % there are 2 dB between levels, go slower
                lager:debug("Power ~s: average rssi ~w, power ~w -> up by ~w",
                    [lorawan_utils:binary_to_hex(Node#node.devaddr), round(AvgRSSI), TxPower, PwrStepUp]),
                min(MinPower, TxPower+PwrStepUp);
            AvgRSSI < -102, TxPower > MaxPower ->
                PwrStepDown = trunc((AvgRSSI+98)/2), % go faster
                lager:debug("Power ~s: average rssi ~w, power ~w -> down by ~w",
                    [lorawan_utils:binary_to_hex(Node#node.devaddr), round(AvgRSSI), TxPower, PwrStepDown]),
                max(MaxPower, TxPower+PwrStepDown); % steps are negative
            true ->
                TxPower
        end,
    % verify if something has changed
    case {TxPower2, DataRate2, Chans} of
        {TxPower, DataRate, Chans} ->
            Node;
        Set ->
            % request ADR command
            Node#node{adr_set=Set}
    end.

send_adr(#network{region=Region},
        #node{adr_flag=1, adr_set={TxPower, DataRate, Chans}, adr_failed=Failed}=Node, FOptsOut)
        when (is_integer(TxPower) or is_integer(DataRate) or is_list(Chans)), (Failed == undefined orelse Failed == []) ->
    Set = merge_adr(Node#node.adr_set, Node#node.adr_use),
    lager:debug("LinkADRReq ~w", [Set]),
    lorawan_mac_region:set_channels(Region, Set, FOptsOut);
send_adr(_Network, _Node, FOptsOut) ->
    % the device has disabled ADR
    FOptsOut.

merge_adr({A1,B1,C1},{A2,B2,C2}) ->
    {if
        is_integer(A1) -> A1;
        true -> A2
    end,
    if
        is_integer(B1) -> B1;
        true -> B2
    end,
    if
        is_list(C1), length(C1) > 0 -> C1;
        true -> C2
    end};
merge_adr(_Else, ABC) ->
    ABC.

set_rxwin(Profile, #node{adr_flag=1, rxwin_failed=Failed}=Node, FOptsOut)
        when Failed == undefined; Failed == [] ->
    case merge_rxwin(Profile#profile.rxwin_set, Node#node.rxwin_use) of
        Unchanged when Unchanged == Node#node.rxwin_use ->
            FOptsOut;
        {OffSet, RX2DataRate, Frequency} ->
            lager:debug("RXParamSetupReq ~w ~w ~w", [OffSet, RX2DataRate, Frequency]),
            [{rx_param_setup_req, OffSet, RX2DataRate, trunc(10000*Frequency)} | FOptsOut]
    end;
set_rxwin(_Profile, _Node, FOptsOut) ->
    FOptsOut.

merge_rxwin({A1,B1,C1},{A2,B2,C2}) ->
    {if
        is_integer(A1) -> A1;
        true -> A2
    end,
    if
        is_integer(B1) -> B1;
        true -> B2
    end,
    if
        is_number(C1) -> C1;
        true -> C2
    end};
merge_rxwin(_Else, ABC) ->
    ABC.

request_status(#profile{request_devstat=false}, _Node, FOptsOut) ->
    FOptsOut;
request_status(_Profile, #node{devstat_time=LastDate, devstat_fcnt=LastFCnt}, FOptsOut)
        when LastDate == undefined; LastFCnt == undefined ->
    [dev_status_req | FOptsOut];
request_status(_Profile, #node{devstat=Stats, devstat_time=LastDate, devstat_fcnt=LastFCnt}=Node, FOptsOut) ->
    {ok, {MaxTime, MaxFCnt}} = application:get_env(lorawan_server, devstat_gap),
    TimeDiff = calendar:datetime_to_gregorian_seconds(calendar:universal_time())
                - calendar:datetime_to_gregorian_seconds(LastDate),
    Divider =
        case Stats of
            [{_Time, Battery, _Margin, _MaxSNR} | _] when Battery < 100 ->
                2;
            _Else ->
                1
        end,
    if
        TimeDiff > MaxTime / Divider;
        Node#node.fcntup - LastFCnt > MaxFCnt / Divider ->
            [dev_status_req | FOptsOut];
        true ->
            FOptsOut
    end.

add_when_zero(Error, 0, List) -> [Error|List];
add_when_zero(_Error, 1, List) -> List.

-include_lib("eunit/include/eunit.hrl").

command_test_()-> [
    % from the LoRaWAN Specification 1.0.3, Section 5.9
    ?_assertEqual({device_time_ans,1139322288000},
        send_device_time([{<<>>, #rxq{time={{2016, 2, 12}, {14, 24, 31}}, tmms=undefined}}]))
].

% end of file
