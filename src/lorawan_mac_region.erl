%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_mac_region).

-export([freq_range/1, default_adr/1, max_adr/1, default_rxwin/1, eirp_limits/1]).
-export([datar_to_dr/2, powe_to_num/2]).
-export([join1_window/2, join2_window/2, rx1_window/2, rx2_window/2, rx2_rf/2, rf_fixed/1]).
-export([max_uplink_snr/1, max_uplink_snr/2, max_downlink_snr/3]).
-export([set_channels/3]).
-export([tx_time/2]).

-include_lib("lorawan_server_api/include/lorawan_application.hrl").

% receive windows

join1_window(Link, RxQ) ->
    tx_window(Link#link.region, join1_delay, RxQ#rxq.tmst, rx1_rf(Link#link.region, RxQ, 0)).

join2_window(Link, RxQ) ->
    tx_window(Link#link.region, join2_delay, RxQ#rxq.tmst, rx2_rf(Link#link.region, RxQ)).

rx1_window(Link, RxQ) ->
    tx_window(Link#link.region, rx1_delay, RxQ#rxq.tmst, rx1_rf(Link, RxQ)).

rx2_window(Link, RxQ) ->
    tx_window(Link#link.region, rx2_delay, RxQ#rxq.tmst, rx2_rf(Link#link.region, RxQ)).

rx1_rf(Link, RxQ) ->
    Offset = case Link#link.rxwin_use of
        {Off, _, _} when is_integer(Off) -> Off;
        _Else -> 0
    end,
    rx1_rf(Link#link.region, RxQ, Offset).

% we calculate in fixed-point numbers
rx1_rf(Region, RxQ, Offset)
        when Region == <<"EU863-870">>; Region == <<"CN779-787">>; Region == <<"EU433">>; Region == <<"KR920-923">>; Region == <<"AS923-JP">> ->
    rf_same(Region, RxQ, RxQ#rxq.freq, Offset);
rx1_rf(<<"US902-928">> = Region, RxQ, Offset) ->
    RxCh = f2ch(RxQ#rxq.freq, {9023, 2}, {9030, 16}),
    rf_same(Region, RxQ, ch2f(Region, RxCh rem 8), Offset);
rx1_rf(<<"US902-928-PR">> = Region, RxQ, Offset) ->
    RxCh = f2ch(RxQ#rxq.freq, {9023, 2}, {9030, 16}),
    rf_same(Region, RxQ, ch2f(Region, RxCh div 8), Offset);
rx1_rf(<<"AU915-928">> = Region, RxQ, Offset) ->
    RxCh = f2ch(RxQ#rxq.freq, {9152, 2}, {9159, 16}),
    rf_same(Region, RxQ, ch2f(Region, RxCh rem 8), Offset);
rx1_rf(<<"CN470-510">> = Region, RxQ, Offset) ->
    RxCh = f2ch(RxQ#rxq.freq, {4703, 2}),
    rf_same(Region, RxQ, ch2f(Region, RxCh rem 48), Offset).

rx2_rf(<<"US902-928-PR">> = Region, RxQ) ->
    RxCh = f2ch(RxQ#rxq.freq, {9023, 2}, {9030, 16}),
    rf_same(Region, RxQ, ch2f(Region, RxCh div 8), 0);
rx2_rf(Region, _RxQ) ->
    rf_fixed(Region).

f2ch(Freq, {Start, Inc}) -> round(10*Freq-Start) div Inc.

% the channels are overlapping, return the integer value
f2ch(Freq, {Start1, Inc1}, _) when round(10*Freq-Start1) rem Inc1 == 0 ->
    round(10*Freq-Start1) div Inc1;
f2ch(Freq, _, {Start2, Inc2}) when round(10*Freq-Start2) rem Inc2 == 0 ->
    64 + round(10*Freq-Start2) div Inc2.

ch2f(<<"EU863-870">>, Ch) ->
    if
        Ch >= 0, Ch =< 2 ->
            ch2fi(Ch, {8681, 2});
        Ch >= 3, Ch =< 7 ->
            ch2fi(Ch, {8671, 2});
        Ch == 8 ->
            868.8
    end;
ch2f(<<"CN779-787">>, Ch) ->
    ch2fi(Ch, {7795, 2});
ch2f(<<"EU433">>, Ch) ->
    ch2fi(Ch, {4331.75, 2});
ch2f(Region, Ch)
        when Region == <<"US902-928">>; Region == <<"US902-928-PR">>; Region == <<"AU915-928">> ->
    ch2fi(Ch, {9233, 6});
ch2f(<<"CN470-510">>, Ch) ->
    ch2fi(Ch, {5003, 2}).

ch2fi(Ch, {Start, Inc}) -> (Ch*Inc + Start)/10.

rf_fixed(Region) ->
    {Freq, DataRate, CodingRate} = regional_config(rx2_rf, Region),
    #txq{region=Region, freq=Freq, datr=DataRate, codr=CodingRate}.

rf_same(Region, RxQ, Freq, Offset) ->
    DataRate = datar_to_down(Region, RxQ#rxq.datr, Offset),
    #txq{region=Region, freq=Freq, datr=DataRate, codr=RxQ#rxq.codr}.

tx_window(<<"US902-928-PR">>=Region, Window, Stamp, TxQ) ->
    Delay = regional_config(Window, Region),
    TxQ#txq{tmst=Stamp+Delay};
tx_window(_Region, Window, Stamp, TxQ) ->
    {ok, Delay} = application:get_env(lorawan_server, Window),
    TxQ#txq{tmst=Stamp+Delay}.

datar_to_down(Region, DataRate, Offset) ->
    DR2 = dr_to_down(Region, datar_to_dr(Region, DataRate), Offset),
    dr_to_datar(Region, DR2).

dr_to_down(Region, DR, Offset) ->
    lists:nth(Offset+1, drs_to_down(Region, DR)).

drs_to_down(Region, DR)
        when Region == <<"EU863-870">>; Region == <<"CN779-787">>; Region == <<"EU433">>;
             Region == <<"CN470-510">>; Region == <<"KR920-923">>; Region == <<"AS923-JP">> ->
    case DR of
        0 -> [0, 0, 0, 0, 0, 0];
        1 -> [1, 0, 0, 0, 0, 0];
        2 -> [2, 1, 0, 0, 0, 0];
        3 -> [3, 2, 1, 0, 0, 0];
        4 -> [4, 3, 2, 1, 0, 0];
        5 -> [5, 4, 3, 2, 1, 0];
        6 -> [6, 5, 4, 3, 2, 1];
        7 -> [7, 6, 5, 4, 3, 2]
    end;
drs_to_down(Region, DR)
        when Region == <<"US902-928">>; Region == <<"US902-928-PR">>; Region == <<"AU915-928">> ->
    case DR of
        0 -> [10, 9,  8,  8];
        1 -> [11, 10, 9,  8];
        2 -> [12, 11, 10, 9];
        3 -> [13, 12, 11, 10];
        4 -> [13, 13, 12, 11]
    end.

% data rate and end-device output power encoding

datars(Region)
        when Region == <<"EU863-870">>; Region == <<"CN779-787">>; Region == <<"EU433">>;
             Region == <<"CN470-510">>; Region == <<"KR920-923">>; Region == <<"AS923-JP">> -> [
    {0, {12, 125}},
    {1, {11, 125}},
    {2, {10, 125}},
    {3, {9, 125}},
    {4, {8, 125}},
    {5, {7, 125}},
    {6, {7, 250}},
    {7, 50000}]; % FSK
datars(Region)
        when Region == <<"US902-928">>; Region == <<"US902-928-PR">>; Region == <<"AU915-928">> -> [
    {0,  {10, 125}},
    {1,  {9, 125}},
    {2,  {8, 125}},
    {3,  {7, 125}},
    {4,  {8, 500}},
    {8,  {12, 500}},
    {9,  {11, 500}},
    {10, {10, 500}},
    {11, {9, 500}},
    {12, {8, 500}},
    {13, {7, 500}}].

dr_to_tuple(Region, DR) ->
    case lists:keyfind(DR, 1, datars(Region)) of
        {_, DataRate} -> DataRate;
        false -> undefined
    end.

dr_to_datar(Region, DR) ->
    tuple_to_datar(dr_to_tuple(Region, DR)).

datar_to_dr(Region, DataRate) ->
    case lists:keyfind(datar_to_tuple(DataRate), 2, datars(Region)) of
        {DR, _} -> DR;
        false -> undefined
    end.

tuple_to_datar({SF, BW}) ->
    <<"SF", (integer_to_binary(SF))/binary, "BW", (integer_to_binary(BW))/binary>>;
tuple_to_datar(DataRate) ->
    DataRate. % FSK

datar_to_tuple(DataRate) when is_binary(DataRate) ->
    [SF, BW] = binary:split(DataRate, [<<"SF">>, <<"BW">>], [global, trim_all]),
    {binary_to_integer(SF), binary_to_integer(BW)};
datar_to_tuple(DataRate) when is_integer(DataRate) ->
    DataRate. % FSK

codr_to_tuple(CodingRate) ->
    [A, B] = binary:split(CodingRate, [<<"/">>], [global, trim_all]),
    {binary_to_integer(A), binary_to_integer(B)}.

powers(Region)
        when Region == <<"EU863-870">> -> [
    {0, 20},
    {1, 14},
    {2, 11},
    {3, 8},
    {4, 5},
    {5, 2}];
powers(Region)
        when Region == <<"US902-928">>; Region == <<"US902-928-PR">>; Region == <<"AU915-928">> -> [
    {0, 30},
    {1, 28},
    {2, 26},
    {3, 24},
    {4, 22},
    {5, 20},
    {6, 18},
    {7, 16},
    {8, 14},
    {9, 12},
    {10, 10}];
powers(Region)
        when Region == <<"CN779-787">>; Region == <<"EU433">> -> [
    {0, 10},
    {1, 7},
    {2, 4},
    {3, 1},
    {4, -2},
    {5, -5}];
powers(Region)
        when Region == <<"CN470-510">> -> [
    {0, 17},
    {1, 16},
    {2, 14},
    {3, 12},
    {4, 10},
    {5, 7},
    {6, 5},
    {7, 2}];
powers(Region)
        when Region == <<"KR920-923">> -> [
    {0, 20},
    {1, 14},
    {2, 10},
    {3, 8},
    {4, 5},
    {5, 2},
    {6, 0}];
powers(Region)
        when Region == <<"AS923-JP">> -> [
    {0, 13},
    {1, 12},
    {2, 10},
    {3, 8},
    {4, 6},
    {5, 4},
    {6, 0}].

powe_to_num(Region, Pow) ->
    case lists:keyfind(Pow, 1, powers(Region)) of
        {_, Power} -> Power;
        false -> undefined
    end.

regional_config(Param, Region) ->
    {ok, Regions} = application:get_env(lorawan_server, regions),
    Config = proplists:get_value(Region, Regions, []),
    proplists:get_value(Param, Config).

% {TXPower, DataRate, Chans}
default_adr(<<"EU863-870">>) -> {1, 0, [{0,2}]};
default_adr(<<"US902-928">>) -> {5, 0, [{0,71}]};
default_adr(<<"US902-928-PR">>) -> {5, 0, [{0,7}]};
default_adr(<<"CN779-787">>) -> {1, 0, [{0,2}]};
default_adr(<<"EU433">>) -> {0, 0, [{0,2}]};
default_adr(<<"AU915-928">>) -> {5, 0, [{0,71}]};
default_adr(<<"CN470-510">>) -> {2, 0, [{0, 95}]};
default_adr(<<"KR920-923">>) -> {1, 0, [{0, 2}]};
default_adr(<<"AS923-JP">>) -> {1, 0, [{0, 1}]}.

% {RX1DROffset, RX2DataRate, Frequency}
default_rxwin(Region) ->
    {Freq, DataRate, _CodingRate} = regional_config(rx2_rf, Region),
    {0, datar_to_dr(Region, DataRate), Freq}.

% {TXPower, DataRate}
max_adr(<<"EU863-870">>) -> {5, 6};
max_adr(<<"US902-928">>) -> {10, 4};
max_adr(<<"US902-928-PR">>) -> {10, 4};
max_adr(<<"CN779-787">>) -> {5, 6};
max_adr(<<"EU433">>) -> {5, 6};
max_adr(<<"AU915-928">>) -> {10, 4};
max_adr(<<"CN470-510">>) -> {7, 6};
max_adr(<<"KR920-923">>) -> {6, 5};
max_adr(<<"AS923-JP">>) -> {6, 5}.

% {default, maximal} power for downlinks (dBm)
eirp_limits(<<"EU863-870">>) -> {14, 20};
eirp_limits(<<"US902-928">>) -> {20, 26};
eirp_limits(<<"US902-928-PR">>) -> {20, 26};
eirp_limits(<<"CN779-787">>) -> {10, 10};
eirp_limits(<<"EU433">>) -> {10, 10};
eirp_limits(<<"AU915-928">>) -> {20, 26};
eirp_limits(<<"CN470-510">>) -> {14, 17};
eirp_limits(<<"KR920-923">>) -> {14, 23};
eirp_limits(<<"AS923-JP">>) -> {13, 13}.

% {Min, Max}
freq_range(<<"EU863-870">>) -> {863, 870};
freq_range(<<"US902-928">>) -> {902, 928};
freq_range(<<"US902-928-PR">>) -> {902, 928};
freq_range(<<"CN779-787">>) -> {779, 787};
freq_range(<<"EU433">>) -> {433, 435};
freq_range(<<"AU915-928">>) -> {915, 928};
freq_range(<<"CN470-510">>) -> {470, 510};
freq_range(<<"KR920-923">>) -> {920, 923};
freq_range(<<"AS923-JP">>) -> {920.6, 923.4}.

max_uplink_snr(DataRate) ->
    {SF, _} = datar_to_tuple(DataRate),
    max_snr(SF).

max_uplink_snr(Region, DataRate) ->
    {SF, _} = dr_to_tuple(Region, DataRate),
    max_snr(SF).

max_downlink_snr(Region, DataRate, Offset) ->
    {SF, _} = dr_to_tuple(Region, dr_to_down(Region, DataRate, Offset)),
    max_snr(SF).

% from SX1272 DataSheet, Table 13
max_snr(SF) ->
    -5-2.5*(SF-6). % dB

% link_adr_req command

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
                append_mask(3, {TXPower, DataRate, Chans}, FOptsOut)]
    end;
set_channels(Region, {TXPower, DataRate, Chans}, FOptsOut)
        when Region == <<"CN470-510">> ->
    case all_bit({0,95}, Chans) of
        true ->
            [{link_adr_req, DataRate, TXPower, 0, 6, 0} | FOptsOut];
        false ->
            append_mask(5, {TXPower, DataRate, Chans}, FOptsOut)
    end.

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

append_mask(Idx, _, FOptsOut) when Idx < 0 ->
    FOptsOut;
append_mask(Idx, {TXPower, DataRate, Chans}, FOptsOut) ->
    append_mask(Idx-1, {TXPower, DataRate, Chans},
        case build_bin(Chans, {16*Idx, 16*(Idx+1)-1}) of
            0 -> FOptsOut;
            ChMask -> [{link_adr_req, DataRate, TXPower, ChMask, Idx, 0} | FOptsOut]
        end).

% transmission time estimation

tx_time(#txdata{data=Data}, TxQ) ->
    tx_time(byte_size(Data), TxQ);
tx_time(Length, #txq{datr=DataRate, codr=CodingRate}) ->
    {SF, BW} = datar_to_tuple(DataRate),
    {4, CR} = codr_to_tuple(CodingRate),
    tx_time(Length, SF, CR, BW*1000).

% see http://www.semtech.com/images/datasheet/LoraDesignGuide_STD.pdf
tx_time(PL, SF, CR, 125000) when SF == 11; SF == 12 ->
    % Optimization is mandated with spreading factors of 11 and 12 at 125 kHz bandwidth
    tx_time(PL, SF, CR, 125000, 1);
tx_time(PL, SF, CR, 250000) when SF == 12 ->
    % The firmware uses also optimization for SF 12 at 250 kHz bandwidth
    tx_time(PL, SF, CR, 250000, 1);
tx_time(PL, SF, CR, BW) ->
    tx_time(PL, SF, CR, BW, 0).

tx_time(PL, SF, CR, BW, DE) ->
    TSym = math:pow(2, SF)/BW,
    % lorawan uses an explicit header
    PayloadSymbNb = 8 + max(ceiling((8*PL-4*SF+28+16)/(4*(SF-2*DE)))*CR, 0),
    % lorawan uses 8 symbols preamble
    % the last +1 is a correction based on practical experiments
    1000*((8 + 4.25) + PayloadSymbNb + 1)*TSym.

ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.

-include_lib("eunit/include/eunit.hrl").

region_test_()-> [
    ?_assertEqual({12,125}, datar_to_tuple(<<"SF12BW125">>)),
    ?_assertEqual({4,6}, codr_to_tuple(<<"4/6">>)),
    % values [ms] verified using the LoRa Calculator, +1 chirp correction based on experiments
    ?_assertEqual(1024, test_tx_time(<<"0123456789">>, <<"SF12BW125">>, <<"4/5">>)),
    ?_assertEqual(297, test_tx_time(<<"0123456789">>, <<"SF10BW125">>, <<"4/5">>)),
    ?_assertEqual(21, test_tx_time(<<"0123456789">>, <<"SF7BW250">>, <<"4/5">>)),
    ?_assertEqual(11, test_tx_time(<<"0123456789">>, <<"SF7BW500">>, <<"4/5">>)),
    ?_assertEqual(dr_to_datar(<<"EU863-870">>, 0), <<"SF12BW125">>),
    ?_assertEqual(dr_to_datar(<<"US902-928">>, 8), <<"SF12BW500">>),
    ?_assertEqual(datar_to_dr(<<"EU863-870">>, <<"SF9BW125">>), 3),
    ?_assertEqual(datar_to_dr(<<"US902-928">>, <<"SF7BW500">>), 13),
    ?_assertEqual(<<"SF10BW500">>, datar_to_down(<<"US902-928">>, <<"SF10BW125">>, 0))].

test_tx_time(Packet, DataRate, CodingRate) ->
    round(tx_time(#txdata{data=Packet},
        % the constants are only to make Dialyzer happy
        #txq{region= <<"EU863-870">>, freq=869.525, datr=DataRate, codr=CodingRate})).

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
    ?_assertEqual([{link_adr_req,<<"SF12BW250">>,14,7,0,0}],
        set_channels(<<"EU863-870">>, {14, <<"SF12BW250">>, [{0, 2}]}, [])),
    ?_assertEqual([{link_adr_req,<<"SF12BW500">>,20,0,7,0}, {link_adr_req,<<"SF12BW500">>,20,255,0,0}],
        set_channels(<<"US902-928">>, {20, <<"SF12BW500">>, [{0, 7}]}, [])),
    ?_assertEqual([{link_adr_req,<<"SF12BW500">>,20,2,7,0}, {link_adr_req,<<"SF12BW500">>,20,65280,0,0}],
        set_channels(<<"US902-928">>, {20, <<"SF12BW500">>, [{8, 15}, {65, 65}]}, []))
].

% end of file
