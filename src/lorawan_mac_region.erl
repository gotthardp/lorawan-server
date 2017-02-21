%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_mac_region).

-export([rx1_rf/3, rx2_rf/3, rx2_dr/1, default_adr/1]).
-export([datar_to_dr/2, freq_range/1, regional_config/2]).

-include("lorawan.hrl").

% we calculate in fixed-point numbers
rx1_rf(Region, RxQ, Window)
        when Region == <<"EU863-870">>; Region == <<"CN779-787">>; Region == <<"EU433">> ->
    rf_same(Region, RxQ, RxQ#rxq.freq, Window);
rx1_rf(<<"US902-928">> = Region, RxQ, Window) ->
    RxCh = f2ch(RxQ#rxq.freq, {9023, 2}, {9030, 16}),
    rf_same(Region, RxQ, ch2f(RxCh rem 8, {9233, 6}), Window);
rx1_rf(<<"US902-928-PR">> = Region, RxQ, Window) ->
    RxCh = f2ch(RxQ#rxq.freq, {9023, 2}, {9030, 16}),
    rf_same(Region, RxQ, ch2f(RxCh div 8, {9233, 6}), Window);
rx1_rf(<<"AU915-928">> = Region, RxQ, Window) ->
    RxCh = f2ch(RxQ#rxq.freq, {9152, 2}, {9159, 16}),
    rf_same(Region, RxQ, ch2f(RxCh rem 8, {9233, 6}), Window);
rx1_rf(<<"CN470-510">> = Region, RxQ, Window) ->
    RxCh = f2ch(RxQ#rxq.freq, {4703, 2}),
    rf_same(Region, RxQ, ch2f(RxCh rem 48, {5003, 2}), Window).

rx2_rf(<<"US902-928-PR">> = Region, RxQ, Window) ->
    RxCh = f2ch(RxQ#rxq.freq, {9023, 2}, {9030, 16}),
    rf_same(Region, RxQ, ch2f(RxCh div 8, {9233, 6}), Window);
rx2_rf(Region, RxQ, Window) ->
    rf_fixed(Region, RxQ, Window).

f2ch(Freq, {Start, Inc}) -> trunc(10*Freq-Start) div Inc.

% the channels are overlapping, return the integer value
f2ch(Freq, {Start1, Inc1}, {Start2, Inc2}) ->
    if
        trunc(10*Freq-Start1) rem Inc1 == 0 -> trunc(10*Freq-Start1) div Inc1;
        trunc(10*Freq-Start2) rem Inc2 == 0 -> 64 + trunc(10*Freq-Start2) div Inc2
    end.

ch2f(Ch, {Start, Inc}) -> (Ch*Inc + Start)/10.

rf_fixed(Region, RxQ, Window) ->
    {Freq, DataRate} = regional_config(rx2_rf, Region),
    Delay = regional_config(Window, Region),
    Power = default_erp(Region),
    #txq{freq=Freq, datr=DataRate, codr=RxQ#rxq.codr, tmst=RxQ#rxq.tmst+Delay, powe=Power}.

rf_same(Region, RxQ, Freq, Window) ->
    % TODO: implement RX1DROffset
    DataRate = datar_to_down(Region, RxQ#rxq.datr, 0),
    Delay = regional_config(Window, Region),
    Power = default_erp(Region),
    #txq{freq=Freq, datr=DataRate, codr=RxQ#rxq.codr, tmst=RxQ#rxq.tmst+Delay, powe=Power}.

datar_to_down(Region, DataRate, Offset) ->
    Down = dr_to_down(Region, datar_to_dr(Region, DataRate)),
    dr_to_datar(Region, lists:nth(Offset+1, Down)).

dr_to_down(Region, DR)
        when Region == <<"EU863-870">>; Region == <<"CN779-787">>; Region == <<"EU433">>; Region == <<"CN470-510">> ->
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
dr_to_down(Region, DR)
        when Region == <<"US902-928">>; Region == <<"US902-928-PR">>; Region == <<"AU915-928">> ->
    case DR of
        0 -> [10, 9,  8,  8];
        1 -> [11, 10, 9,  8];
        2 -> [12, 11, 10, 9];
        3 -> [13, 12, 11, 10];
        4 -> [13, 13, 12, 11]
    end.

% data rate conversions

rx2_dr(Region) ->
    {_F, DataRate} = regional_config(rx2_rf, Region),
    datar_to_dr(Region, DataRate).

datars(Region)
        when Region == <<"EU863-870">>; Region == <<"CN779-787">>; Region == <<"EU433">>; Region == <<"CN470-510">> -> [
    {0, {12, 125}},
    {1, {11, 125}},
    {2, {10, 125}},
    {3, {9, 125}},
    {4, {8, 125}},
    {5, {7, 125}},
    {6, {7, 250}}];
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

dr_to_datar(Region, DR) ->
    {_, DataRate} = lists:keyfind(DR, 1, datars(Region)),
    tuple_to_datar(DataRate).

datar_to_dr(Region, DataRate) ->
    {DR, _} = lists:keyfind(datar_to_tuple(DataRate), 2, datars(Region)),
    DR.

tuple_to_datar({SF, BW}) ->
    <<"SF", (integer_to_binary(SF))/binary, "BW", (integer_to_binary(BW))/binary>>.

datar_to_tuple(DataRate) ->
    [SF, BW] = binary:split(DataRate, [<<"SF">>, <<"BW">>], [global, trim_all]),
    {binary_to_integer(SF), binary_to_integer(BW)}.

regional_config(Param, Region) ->
    {ok, Regions} = application:get_env(regions),
    Config = proplists:get_value(Region, Regions, []),
    proplists:get_value(Param, Config).

% {TXPower, DataRate, Chans}
default_adr(<<"EU863-870">>) -> {1, 0, [{0,2}]};
default_adr(<<"US902-928">>) -> {5, 0, [{0,71}]};
default_adr(<<"US902-928-PR">>) -> {5, 0, [{0,7}]};
default_adr(<<"CN779-787">>) -> {1, 0, [{0,2}]};
default_adr(<<"EU433">>) -> {0, 0, [{0,2}]};
default_adr(<<"AU915-928">>) -> {5, 0, [{0,71}]};
default_adr(<<"CN470-510">>) -> {2, 0, [{0, 95}]}.

% default power for downlinks (dBm)
default_erp(<<"EU863-870">>) -> 14;
default_erp(<<"US902-928">>) -> 20;
default_erp(<<"US902-928-PR">>) -> 20;
default_erp(<<"CN779-787">>) -> 10;
default_erp(<<"EU433">>) -> 10;
default_erp(<<"AU915-928">>) -> 20;
default_erp(<<"CN470-510">>) -> 14.

% {Min, Max}
freq_range(<<"EU863-870">>) -> {863, 870};
freq_range(<<"US902-928">>) -> {902, 928};
freq_range(<<"US902-928-PR">>) -> {902, 928};
freq_range(<<"CN779-787">>) -> {779, 787};
freq_range(<<"EU433">>) -> {433, 435};
freq_range(<<"AU915-928">>) -> {915, 928};
freq_range(<<"CN470-510">>) -> {470, 510}.

-include_lib("eunit/include/eunit.hrl").

region_test_()-> [
    ?_assertEqual(dr_to_datar(<<"EU863-870">>, 0), <<"SF12BW125">>),
    ?_assertEqual(dr_to_datar(<<"US902-928">>, 8), <<"SF12BW500">>),
    ?_assertEqual(datar_to_dr(<<"EU863-870">>, <<"SF9BW125">>), 3),
    ?_assertEqual(datar_to_dr(<<"US902-928">>, <<"SF7BW500">>), 13),
    ?_assertEqual(<<"SF10BW500">>, datar_to_down(<<"US902-928">>, <<"SF10BW125">>, 0))].

% end of file
