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
parse_fopts(<<16#06, Battery:8, _RFU:2, Margin:6/signed, Rest/binary>>) ->
    [{dev_status_ans, Battery, Margin} | parse_fopts(Rest)];
parse_fopts(<<16#07, _RFU:6, DataRateRangeOK:1, ChannelFreqOK:1, Rest/binary>>) ->
    [{new_channel_ans, DataRateRangeOK, ChannelFreqOK} | parse_fopts(Rest)];
parse_fopts(<<16#0A, _RFU:6, UplinkFreqExists:1, ChannelFreqOK:1, Rest/binary>>) ->
    [{di_channel_ans, UplinkFreqExists, ChannelFreqOK} | parse_fopts(Rest)];
parse_fopts(<<16#08, Rest/binary>>) ->
    [rx_timing_setup_ans | parse_fopts(Rest)];
parse_fopts(<<16#09, Rest/binary>>) ->
    [tx_param_setup_ans | parse_fopts(Rest)];
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
    <<16#04, 0:4, MaxDCycle:4, (encode_fopts(Rest))/binary>>;
encode_fopts([{rx_param_setup_req, RX1DROffset, RX2DataRate, Frequency} | Rest]) ->
    <<16#05, 0:1, RX1DROffset:3, RX2DataRate:4, Frequency:24/little-unsigned-integer, (encode_fopts(Rest))/binary>>;
encode_fopts([dev_status_req | Rest]) ->
    <<16#06, (encode_fopts(Rest))/binary>>;
encode_fopts([{new_channel_req, ChIndex, Freq, MaxDR, MinDR} | Rest]) ->
    <<16#07, ChIndex, Freq:24/little-unsigned-integer, MaxDR:4, MinDR:4, (encode_fopts(Rest))/binary>>;
encode_fopts([{di_channel_req, ChIndex, Freq} | Rest]) ->
    <<16#0A, ChIndex, Freq:24/little-unsigned-integer, (encode_fopts(Rest))/binary>>;
encode_fopts([{rx_timing_setup_req, Delay} | Rest]) ->
    <<16#08, 0:4, Delay:4, (encode_fopts(Rest))/binary>>;
encode_fopts([{tx_param_setup_req, DownDwell, UplinkDwell, MaxEIRP} | Rest]) ->
    <<16#09, 0:2, DownDwell:1, UplinkDwell:1, MaxEIRP:4, (encode_fopts(Rest))/binary>>;
encode_fopts([]) ->
    <<>>.


handle_adr(FOptsIn, Link) ->
    case find_adr(FOptsIn) of
        {1, 1, 1} when Link#link.adr_set == Link#link.adr_use ->
            lager:debug("LinkADRReq ~s succeeded (enforcement only)",
                [lorawan_mac:binary_to_hex(Link#link.devaddr)]),
            % the desired ADR is already used
            Link#link{adr_flag_set=update_flag_set(Link#link.adr_flag_set)};
        {1, 1, 1} ->
            lager:debug("LinkADRReq ~s succeeded",
                [lorawan_mac:binary_to_hex(Link#link.devaddr)]),
            Link#link{adr_flag_set=update_flag_set(Link#link.adr_flag_set),
                adr_use=Link#link.adr_set, devstat_fcnt=undefined, last_qs=[]};
        {PowerACK, DataRateACK, ChannelMaskACK} ->
            lorawan_utils:throw_warning({node, Link#link.devaddr},
                {adr_req_failed, {PowerACK, DataRateACK, ChannelMaskACK}}),
            {TXPower, DataRate, Chans} = Link#link.adr_set,
            % clear the settings that failed
            Link#link{adr_flag_set=update_flag_set(Link#link.adr_flag_set),
                adr_set = {clear_when_zero(PowerACK, TXPower), clear_when_zero(DataRateACK, DataRate),
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

update_flag_set(3) -> 1; % Set, then Auto-Adjust
update_flag_set(4) -> 0; % Set, then Disable
update_flag_set(Else) -> Else.

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
            % compute a maximal D/L SNR
            {_, DataRate, _} = Link#link.adr_use,
            OffUse = case Link#link.rxwin_use of
                {Num2, _, _} when is_number(Num2) -> Num2;
                _Else2 -> 0
            end,
            MaxSNR = lorawan_mac_region:max_downlink_snr(Link#link.region, DataRate, OffUse),
            lager:debug("DevStatus: battery ~B, margin: ~B (max ~.1f)", [Battery, Margin, MaxSNR]),
            Link#link{devstat_time=calendar:universal_time(), devstat_fcnt=Link#link.fcntup,
                devstat=append_status({calendar:universal_time(), Battery, Margin, MaxSNR}, Link#link.devstat)};
        undefined ->
            Link
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

send_link_check(#rxq{datr=DataRate, lsnr=SNR}) ->
    Margin = trunc(SNR - lorawan_mac_region:max_uplink_snr(DataRate)),
    lager:debug("LinkCheckAns: margin: ~B", [Margin]),
    {link_check_ans, Margin, 1}.


auto_adr(RxQ, #link{adr_flag_use=1, adr_flag_set=1}=Link, RxFrame) ->
    % ADR is Auto-Adjust, so maintain quality statistics
    LastQs = appendq({RxQ#rxq.rssi, RxQ#rxq.lsnr}, Link#link.last_qs),
    auto_adr0(Link#link{last_qs=LastQs}, RxFrame);
auto_adr(_RxQ, Link, RxFrame) ->
    % ADR is Manual or OFF
    {Link, RxFrame}.

appendq(SNR, undefined) ->
    [SNR];
appendq(SNR, LastSNRs) ->
    lists:sublist([SNR | LastSNRs], 20).

auto_adr0(#link{last_qs=LastQs, adr_use={TxPower, DataRate, _}, adr_set={_, _, Chans}}=Link, RxFrame)
        when length(LastQs) >= 20, is_integer(TxPower), is_integer(DataRate), is_list(Chans) ->
    {AvgRSSI, AvgSNR} = AverageQs = average(lists:unzip(LastQs)),
    {DefPower, _, _} = lorawan_mac_region:default_adr(Link#link.region),
    {MinPower, MaxDR} = lorawan_mac_region:max_adr(Link#link.region),
    % how many SF steps (per Table 13) are between current SNR and current sensitivity?
    % there is 2.5 dB between the DR, so divide by 3 to get more margin
    MaxSNR = lorawan_mac_region:max_uplink_snr(Link#link.region, DataRate)+10,
    StepsDR = trunc((AvgSNR-MaxSNR)/3),
    DataRate2 = if
            StepsDR > 0, DataRate < MaxDR ->
                lager:debug("DataRate ~s: average snr ~w ~w = ~w, dr ~w -> step ~w",
                    [lorawan_mac:binary_to_hex(Link#link.devaddr), round(AvgSNR), MaxSNR, round(AvgSNR-MaxSNR), DataRate, StepsDR]),
                min(DataRate+StepsDR, MaxDR);
            true ->
                DataRate
        end,
    % receiver sensitivity for maximal DR in all regions is -120 dBm, we try to stay at -100 dBm
    TxPower2 = if
            AvgRSSI > -96, TxPower < MinPower ->
                PwrStepUp = trunc((AvgRSSI+100)/4), % there are 2 dB between levels, go slower
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
    {Link#link{adr_set={TxPower2, DataRate2, Chans}}, RxFrame#rxframe{average_qs=AverageQs}};
auto_adr0(Link, RxFrame) ->
    {Link, RxFrame#rxframe{average_qs=undefined}}.

average({List1, List2}) ->
    {average0(List1), average0(List2)}.

average0(List) ->
    Avg = lists:sum(List)/length(List),
    Sigma = math:sqrt(lists:sum([(N-Avg)*(N-Avg) || N <- List])/length(List)),
    Avg-Sigma.

send_adr(#link{adr_flag_use=1}=Link, FOptsOut) ->
    case Link of
        #link{adr_flag_set=Flag, adr_set={TxPower, DataRate, Chans}}
                when (Flag == 3 orelse Flag == 4),
                is_integer(TxPower), is_integer(DataRate), is_list(Chans) ->
            % the ADR parameters will be enforced
            send_adr0(Link, FOptsOut);
        #link{adr_set=NotChanged, adr_use=NotChanged} ->
            FOptsOut;
        #link{adr_flag_set=Flag, adr_set={TxPower, DataRate, Chans1}, adr_use={TxPower, DataRate, _Chans2}}
                when Flag >= 1,
                is_integer(TxPower), is_integer(DataRate), is_list(Chans1) ->
            % only the channels changed
            send_adr0(Link, FOptsOut);
        #link{adr_flag_set=Flag, adr_set={TxPower, DataRate, Chans}, last_qs=LastQs}
                when ((Flag == 1 andalso length(LastQs) >= 20) orelse Flag == 2),
                is_integer(TxPower), is_integer(DataRate), is_list(Chans) ->
            % the ADR parameters changed
            send_adr0(Link, FOptsOut);
        _Else ->
            FOptsOut
    end;
send_adr(_Link, FOptsOut) ->
    % the device has disabled ADR
    FOptsOut.

send_adr0(#link{region=Region, adr_set=Set}, FOptsOut) ->
    lager:debug("LinkADRReq ~w", [Set]),
    lorawan_mac_region:set_channels(Region, Set, FOptsOut).

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
        Link#link.adr_flag_use == 1, Link#link.adr_flag_set >= 1,
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

clear_when_zero(0, _Value) -> undefined;
clear_when_zero(_Else, Value) -> Value.

% end of file
