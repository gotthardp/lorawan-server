%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_mac_commands).

-export([handle/4, build_fopts/1]).

-include_lib("lorawan_server_api/include/lorawan_application.hrl").

handle(RxQ, Link, FOpts, RxFrame) ->
    FOptsIn = parse_fopts(FOpts),
    case FOptsIn of
        [] -> ok;
        List1 -> lager:debug("~s -> ~w", [lorawan_mac:binary_to_hex(Link#link.devaddr), List1])
    end,
    % process incoming responses
    {MacConfirm, Link2} = handle_rxwin(FOptsIn,
        handle_adr(FOptsIn,
        handle_status(FOptsIn, Link))),
    {Link3, RxFrame2} = auto_adr(RxQ, Link2, RxFrame),
    % process requests
    FOptsOut =
        lists:foldl(
            fun (link_check_req, Acc) -> [send_link_check(RxQ) | Acc];
                (_Else, Acc) -> Acc
            end,
            [], FOptsIn),
    % check for new requests
    {ok, MacConfirm, Link3, build_fopts(Link3, FOptsOut), RxFrame2}.

build_fopts(Link) ->
    build_fopts(Link, []).

build_fopts(Link, FOptsOut0) ->
    FOptsOut = send_adr(Link,
        set_rxwin(Link,
        request_status(Link, FOptsOut0))),
    case FOptsOut of
        [] -> ok;
        List2 -> lager:debug("~s <- ~w", [lorawan_mac:binary_to_hex(Link#link.devaddr), List2])
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
parse_fopts(<<16#06, Battery:8, _RFU:2, Margin:6, Rest/binary>>) ->
    [{dev_status_ans, Battery, Margin} | parse_fopts(Rest)];
parse_fopts(<<16#07, _RFU:6, DataRateRangeOK:1, ChannelFreqOK:1, Rest/binary>>) ->
    [{new_channel_ans, DataRateRangeOK, ChannelFreqOK} | parse_fopts(Rest)];
parse_fopts(<<16#08, Rest/binary>>) ->
    [rx_timing_setup_ans | parse_fopts(Rest)];
parse_fopts(<<>>) ->
    [];
parse_fopts(Unknown) ->
    lager:warning("Unknown command ~p", [Unknown]),
    [].

encode_fopts([{link_check_ans, Margin, GwCnt} | Rest]) ->
    <<16#02, Margin, GwCnt, (encode_fopts(Rest))/binary>>;
encode_fopts([{link_adr_req, DataRate, TXPower, ChMask, ChMaskCntl, NbRep} | Rest]) ->
    <<16#03, DataRate:4, TXPower:4, ChMask:16/little-unsigned-integer, 0:1, ChMaskCntl:3, NbRep:4, (encode_fopts(Rest))/binary>>;
encode_fopts([{duty_cycle_req, MaxDCycle} | Rest]) ->
    <<16#04, MaxDCycle, (encode_fopts(Rest))/binary>>;
encode_fopts([{rx_param_setup_req, RX1DROffset, RX2DataRate, Frequency} | Rest]) ->
    <<16#05, 0:1, RX1DROffset:3, RX2DataRate:4, Frequency:24/little-unsigned-integer, (encode_fopts(Rest))/binary>>;
encode_fopts([dev_status_req | Rest]) ->
    <<16#06, (encode_fopts(Rest))/binary>>;
encode_fopts([{new_channel_req, ChIndex, Freq, MaxDR, MinDR} | Rest]) ->
    <<16#07, ChIndex, Freq:24/little-unsigned-integer, MaxDR:4, MinDR:4, (encode_fopts(Rest))/binary>>;
encode_fopts([{rx_timing_setup_req, Delay} | Rest]) ->
    <<16#08, 0:4, Delay:4, (encode_fopts(Rest))/binary>>;
encode_fopts([]) ->
    <<>>.


handle_adr(FOptsIn, Link) ->
    case find_adr(FOptsIn) of
        {1, 1, 1} ->
            lager:debug("LinkADRReq ~s succeeded", [lorawan_mac:binary_to_hex(Link#link.devaddr)]),
            Link#link{adr_use=Link#link.adr_set, devstat_fcnt=undefined, last_qs=[]};
        {PowerACK, DataRateACK, ChannelMaskACK} ->
            lorawan_utils:throw_warning({node, Link#link.devaddr}, {adr_req_failed, {PowerACK, DataRateACK, ChannelMaskACK}}),
            {TXPower, DataRate, Chans} = Link#link.adr_set,
            % clear the settings that failed
            Link#link{adr_set = {clear_when_zero(PowerACK, TXPower), clear_when_zero(DataRateACK, DataRate),
                clear_when_zero(ChannelMaskACK, Chans)}};
        undefined ->
            Link
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

handle_rxwin(FOptsIn, Link) ->
    case find_rxwin(FOptsIn) of
        {1, 1, 1} ->
            lager:debug("RXParamSetupAns ~s succeeded", [lorawan_mac:binary_to_hex(Link#link.devaddr)]),
            {RX1DROffset, _, _} = Link#link.rxwin_set,
            {_, RX2DataRate, Frequency} = lorawan_mac_region:default_rxwin(Link#link.region),
            {true, Link#link{rxwin_use={RX1DROffset, RX2DataRate, Frequency},
                rxwin_set={RX1DROffset, RX2DataRate, Frequency}}};
        {RX1DROffsetACK, RX2DataRateACK, ChannelACK} ->
            lorawan_utils:throw_warning({node, Link#link.devaddr}, {rxwin_setup_failed, {RX1DROffsetACK, RX2DataRateACK, ChannelACK}}),
            {RX1DROffset, RX2DataRate, Frequency} = Link#link.rxwin_set,
            % clear the settings that failed
            {true, Link#link{rxwin_set = {clear_when_zero(RX1DROffset, RX1DROffsetACK),
                clear_when_zero(RX2DataRate, RX2DataRateACK),
                clear_when_zero(Frequency, ChannelACK)}}};
        undefined ->
            {false, Link}
    end.

find_rxwin(FOptsIn) ->
    lists:foldl(
        fun ({rx_param_setup_ans, RX1DROffsetACK, RX2DataRateACK, ChannelACK}, _Prev) -> {RX1DROffsetACK, RX2DataRateACK, ChannelACK};
            (_Else, Last) -> Last
        end,
        undefined, FOptsIn).

handle_status(FOptsIn, Link) ->
    case find_status(FOptsIn) of
        {Battery, Margin} ->
            lager:debug("DevStatus: battery ~B, margin: ~B", [Battery, Margin-32]),
            Link#link{devstat_time=calendar:universal_time(), devstat_fcnt=Link#link.fcntup, devstat={Battery, Margin-32}};
        undefined ->
            Link
    end.

find_status(FOptsIn) ->
    lists:foldl(
        fun ({dev_status_ans, Battery, Margin}, _Prev) -> {Battery, Margin};
            (_Else, Last) -> Last
        end,
        undefined, FOptsIn).

send_link_check(#rxq{datr=DataRate, lsnr=SNR}) ->
    {SF, _} = lorawan_mac_region:datar_to_tuple(DataRate),
    Margin = trunc(SNR - max_snr(SF)),
    lager:debug("LinkCheckAns: margin: ~B", [Margin]),
    {link_check_ans, Margin, 1}.


auto_adr(RxQ, #link{adr_flag_use=1, adr_flag_set=1}=Link, RxFrame) ->
    % ADR is ON, so maintain quality statistics
    LastQs = appendq({RxQ#rxq.rssi, RxQ#rxq.lsnr}, Link#link.last_qs),
    auto_adr0(Link#link{last_qs=LastQs}, RxFrame);
auto_adr(_RxQ, Link, RxFrame) ->
    % ADR is Manual or OFF
    {Link, RxFrame}.

appendq(SNR, undefined) ->
    [SNR];
appendq(SNR, LastSNRs) ->
    lists:sublist([SNR | LastSNRs], 20).

auto_adr0(#link{last_qs=LastQs}=Link, RxFrame) when length(LastQs) >= 20 ->
    {AvgRSSI, AvgSNR} = AverageQs = average(lists:unzip(LastQs)),
    {DefPower, _, _} = lorawan_mac_region:default_adr(Link#link.region),
    {MinPower, MaxDR} = lorawan_mac_region:max_adr(Link#link.region),
    {TxPower, DataRate, _} = Link#link.adr_use,
    % how many SF steps (per Table 13) are between current SNR and current sensitivity?
    % there is 2.5 dB between the DR, so divide by 3 to get more margin
    MaxSNR = max_snr(Link#link.region, DataRate)+10,
    StepsDR = trunc((AvgSNR-MaxSNR)/3),
    DataRate2 = if
            DataRate == undefined ->
                DataRate;
            StepsDR > 0, DataRate < MaxDR ->
                lager:debug("DataRate ~s: average snr ~w ~w = ~w, dr ~w -> step ~w",
                    [lorawan_mac:binary_to_hex(Link#link.devaddr), round(AvgSNR), MaxSNR, round(AvgSNR-MaxSNR), DataRate, StepsDR]),
                min(DataRate+StepsDR, MaxDR);
            true ->
                DataRate
        end,
    % receiver sensitivity for maximal DR in all regions is -120 dBm, we try to stay at -100 dBm
    TxPower2 = if
            TxPower == undefined ->
                TxPower;
            AvgRSSI > -96, TxPower < MinPower ->
                PwrStepUp = trunc((AvgRSSI+100)/4), % 2-3dB between levels, go slower
                lager:debug("Power ~s: average rssi ~w, power ~w -> up by ~w",
                    [lorawan_mac:binary_to_hex(Link#link.devaddr), round(AvgRSSI), TxPower, PwrStepUp]),
                min(MinPower, TxPower+PwrStepUp);
            AvgRSSI < -102, TxPower > DefPower ->
                PwrStepDown = trunc((AvgRSSI+98)/2), % go faster
                lager:debug("Power ~s: average rssi ~w, power ~w -> down by ~w",
                    [lorawan_mac:binary_to_hex(Link#link.devaddr), round(AvgRSSI), TxPower, PwrStepDown]),
                max(DefPower, TxPower+PwrStepDown); % steps are negative
            true ->
                TxPower
        end,
    {_, _, Chans} = Link#link.adr_set,
    {Link#link{adr_set={TxPower2, DataRate2, Chans}}, RxFrame#rxframe{average_qs=AverageQs}};
auto_adr0(Link, RxFrame) ->
    {Link, RxFrame#rxframe{average_qs=undefined}}.

average({List1, List2}) ->
    {average0(List1), average0(List2)}.

average0(List) ->
    Avg = lists:sum(List)/length(List),
    Sigma = math:sqrt(lists:sum([(N-Avg)*(N-Avg) || N <- List])/length(List)),
    Avg-Sigma.

max_snr(Region, DataRate) ->
    {SF, _} = lorawan_mac_region:dr_to_tuple(Region, DataRate),
    max_snr(SF).

% from SX1272 DataSheet, Table 13
max_snr(SF) ->
    -5-2.5*(SF-6). % dB

send_adr(Link, FOptsOut) ->
    IsIncomplete = has_undefined_field(Link#link.adr_set),
    if
        Link#link.adr_flag_use == 1,
        ((Link#link.adr_flag_set == 1 andalso length(Link#link.last_qs) >= 20) orelse Link#link.adr_flag_set == 2),
        not IsIncomplete, Link#link.adr_use /= Link#link.adr_set ->
            lager:debug("LinkADRReq ~w", [Link#link.adr_set]),
            set_channels(Link#link.region, Link#link.adr_set, FOptsOut);
        true ->
            FOptsOut
    end.

set_channels(Region, {TXPower, DataRate, Chans}, FOptsOut)
        when Region == <<"EU863-870">>; Region == <<"CN779-787">>; Region == <<"EU433">>; Region == <<"KR920-923">>; Region == <<"AS923-JP">> ->
    [{link_adr_req, DataRate, TXPower, build_bin(Chans, {0, 15}), 0, 0} | FOptsOut];
set_channels(Region, {TXPower, DataRate, Chans}, FOptsOut)
        when Region == <<"US902-928">>; Region == <<"US902-928-PR">>; Region == <<"AU915-928">> ->
    case all_bit({0,63}, Chans) of
        true ->
            [{link_adr_req, DataRate, TXPower, build_bin(Chans, {64, 71}), 6, 0} | FOptsOut];
        false ->
            [{link_adr_req, DataRate, TXPower, build_bin(Chans, {64, 71}), 7, 0} |
                append_mask(4, {TXPower, DataRate, Chans}, FOptsOut)]
    end;
set_channels(Region, {TXPower, DataRate, Chans}, FOptsOut)
        when Region == <<"CN470-510">> ->
    case all_bit({0,95}, Chans) of
        true ->
            [{link_adr_req, DataRate, TXPower, 0, 6, 0} | FOptsOut];
        false ->
            append_mask(5, {TXPower, DataRate, Chans}, FOptsOut)
    end.

append_mask(Idx, _, FOptsOut) when Idx < 0 ->
    FOptsOut;
append_mask(Idx, {TXPower, DataRate, Chans}, FOptsOut) ->
    append_mask(Idx-1, {TXPower, DataRate, Chans},
        case build_bin(Chans, {16*Idx, 16*(Idx+1)-1}) of
            0 -> FOptsOut;
            ChMask -> [{link_adr_req, DataRate, TXPower, ChMask, Idx, 0} | FOptsOut]
        end).

set_rxwin(Link, FOptsOut) ->
    OffSet = case Link#link.rxwin_set of
        {Num1, _, _} when is_number(Num1) -> Num1;
        _Else1 -> undefined
    end,
    OffUse = case Link#link.rxwin_use of
        {Num2, _, _} when is_number(Num2) -> Num2;
        _Else2 -> undefined
    end,
    if
        Link#link.adr_flag_use == 1,
        (Link#link.adr_flag_set == 1 orelse Link#link.adr_flag_set == 2),
        OffSet /= undefined, OffSet /= OffUse ->
            {_, RX2DataRate, Frequency} = lorawan_mac_region:default_rxwin(Link#link.region),
            lager:debug("RXParamSetupReq ~w", [OffSet]),
            [{rx_param_setup_req, OffSet, RX2DataRate, trunc(10000*Frequency)} | FOptsOut];
        true ->
            FOptsOut
    end.

request_status(#link{request_devstat=false}, FOptsOut) ->
    FOptsOut;
request_status(#link{devstat_time=LastDate, devstat_fcnt=LastFCnt}, FOptsOut)
        when LastDate == undefined; LastFCnt == undefined ->
    [dev_status_req | FOptsOut];
request_status(#link{devstat_time=LastDate, devstat_fcnt=LastFCnt}=Link, FOptsOut) ->
    {ok, {MaxTime, MaxFCnt}} = application:get_env(lorawan_server, devstat_gap),
    TimeDiff = calendar:datetime_to_gregorian_seconds(calendar:universal_time())
                - calendar:datetime_to_gregorian_seconds(LastDate),
    if
        TimeDiff > MaxTime;
        Link#link.fcntup - LastFCnt > MaxFCnt ->
            [dev_status_req | FOptsOut];
        true ->
            FOptsOut
    end.

has_undefined_field(undefined) ->
    true;
has_undefined_field(Tuple) ->
    lists:member(undefined, tuple_to_list(Tuple)).

% bits handling

some_bit(MinMax, Chans) ->
    lists:any(
        fun(Tuple) -> match_part(MinMax, Tuple) end, Chans).

all_bit(MinMax, Chans) ->
    lists:any(
        fun(Tuple) -> match_whole(MinMax, Tuple) end, Chans).

none_bit(MinMax, Chans) ->
    lists:all(
        fun(Tuple) -> not match_part(MinMax, Tuple) end, Chans).

match_part(MinMax, {A,B}) when B < A ->
    match_part(MinMax, {B,A});
match_part({Min, Max}, {A,B}) ->
    (A =< Max) and (B >= Min).

match_whole(MinMax, {A,B}) when B < A ->
    match_whole(MinMax, {B,A});
match_whole({Min, Max}, {A,B}) ->
    (A =< Min) and (B >= Max).

build_bin(Chans, {Min, Max}) ->
    Bits = Max-Min+1,
    lists:foldl(
        fun(Tuple, Acc) ->
            <<Num:Bits>> = build_bin0({Min, Max}, Tuple),
            Num bor Acc
        end, 0, Chans).

build_bin0(MinMax, {A, B}) when B < A ->
    build_bin0(MinMax, {B, A});
build_bin0({Min, Max}, {A, B}) when B < Min; Max < A ->
    % out of range
    <<0:(Max-Min+1)>>;
build_bin0({Min, Max}, {A, B}) ->
    C = max(Min, A),
    D = min(Max, B),
    Bits = Max-Min+1,
    % construct the binary
    Bin = <<-1:(D-C+1), 0:(C-Min)>>,
    case bit_size(Bin) rem Bits of
        0 -> Bin;
        N -> <<0:(Bits-N), Bin/bits>>
    end.

% auxiliary functions

clear_when_zero(0, _Value) -> undefined;
clear_when_zero(_Else, Value) -> Value.

-include_lib("eunit/include/eunit.hrl").

bits_test_()-> [
    ?_assertEqual(7, build_bin([{0,2}], {0,15})),
    ?_assertEqual(0, build_bin([{0,2}], {16,31})),
    ?_assertEqual(65535, build_bin([{0,71}], {0,15})),
    ?_assertEqual(true, some_bit({0, 71}, [{0,71}])),
    ?_assertEqual(true, all_bit({0, 71}, [{0,71}])),
    ?_assertEqual(false, none_bit({0, 71}, [{0,71}])),
    ?_assertEqual(true, some_bit({0, 15}, [{0,2}])),
    ?_assertEqual(false, all_bit({0, 15}, [{0,2}])),
    ?_assertEqual(false, none_bit({0, 15}, [{0,2}])),
    ?_assertEqual([{link_adr_req,<<"SF12BW250">>,14,7,0,0}], set_channels(<<"EU863-870">>, {14, <<"SF12BW250">>, [{0, 2}]}, [])),
    ?_assertEqual([{link_adr_req,<<"SF12BW500">>,20,0,7,0},
        {link_adr_req,<<"SF12BW500">>,20,255,0,0}], set_channels(<<"US902-928">>, {20, <<"SF12BW500">>, [{0, 7}]}, []))
].

% end of file
