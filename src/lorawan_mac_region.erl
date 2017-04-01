%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_mac_region).

-export([rx1_rf/3, rx2_rf/2, rx2_rf/3, rx2_dr/1, rf_group/1, default_adr/1, max_adr/1]).
-export([dr_to_tuple/2, datar_to_dr/2, freq_range/1, datar_to_tuple/1, powe_to_num/2, regional_config/2]).

-include_lib("lorawan_server_api/include/lorawan_application.hrl").

rx1_rf(Region, RxQ, Window) ->
    tx_time(Region, Window, RxQ#rxq.tmst, rx1_rf(Region, RxQ)).

rx2_rf(Region, RxQ, Window) ->
    tx_time(Region, Window, RxQ#rxq.tmst, rx2_rf(Region, RxQ)).

% we calculate in fixed-point numbers
rx1_rf(Region, RxQ)
        when Region == <<"EU863-870">>; Region == <<"CN779-787">>; Region == <<"EU433">>; Region == <<"KR920-923">> ->
    rf_same(Region, RxQ, RxQ#rxq.freq);
rx1_rf(<<"US902-928">> = Region, RxQ) ->
    RxCh = f2ch(RxQ#rxq.freq, {9023, 2}, {9030, 16}),
    rf_same(Region, RxQ, ch2f(Region, RxCh rem 8));
rx1_rf(<<"US902-928-PR">> = Region, RxQ) ->
    RxCh = f2ch(RxQ#rxq.freq, {9023, 2}, {9030, 16}),
    rf_same(Region, RxQ, ch2f(Region, RxCh div 8));
rx1_rf(<<"AU915-928">> = Region, RxQ) ->
    RxCh = f2ch(RxQ#rxq.freq, {9152, 2}, {9159, 16}),
    rf_same(Region, RxQ, ch2f(Region, RxCh rem 8));
rx1_rf(<<"CN470-510">> = Region, RxQ) ->
    RxCh = f2ch(RxQ#rxq.freq, {4703, 2}),
    rf_same(Region, RxQ, ch2f(Region, RxCh rem 48)).

rx2_rf(<<"US902-928-PR">> = Region, RxQ) ->
    RxCh = f2ch(RxQ#rxq.freq, {9023, 2}, {9030, 16}),
    rf_same(Region, RxQ, ch2f(Region, RxCh div 8));
rx2_rf(Region, RxQ) ->
    rf_fixed(Region, RxQ).

f2ch(Freq, {Start, Inc}) -> trunc(10*Freq-Start) div Inc.

% the channels are overlapping, return the integer value
f2ch(Freq, {Start1, Inc1}, {Start2, Inc2}) ->
    if
        trunc(10*Freq-Start1) rem Inc1 == 0 -> trunc(10*Freq-Start1) div Inc1;
        trunc(10*Freq-Start2) rem Inc2 == 0 -> 64 + trunc(10*Freq-Start2) div Inc2
    end.

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

rf_fixed(Region, RxQ) ->
    {Freq, DataRate} = regional_config(rx2_rf, Region),
    Power = default_erp(Region),
    #txq{freq=Freq, datr=DataRate, codr=RxQ#rxq.codr, powe=Power}.

rf_same(Region, RxQ, Freq) ->
    % TODO: implement RX1DROffset
    DataRate = datar_to_down(Region, RxQ#rxq.datr, 0),
    Power = default_erp(Region),
    #txq{freq=Freq, datr=DataRate, codr=RxQ#rxq.codr, powe=Power}.

rf_group(Group) ->
    #txq{freq = ch2f(Group#multicast_group.region, Group#multicast_group.chan),
        datr = dr_to_datar(Group#multicast_group.region, Group#multicast_group.datr),
        codr = Group#multicast_group.datr,
        powe = default_erp(Group#multicast_group.region)}.

tx_time(Region, Window, Stamp, TxQ) ->
    Delay = regional_config(Window, Region),
    TxQ#txq{tmst=Stamp+Delay}.

datar_to_down(Region, DataRate, Offset) ->
    Down = dr_to_down(Region, datar_to_dr(Region, DataRate)),
    dr_to_datar(Region, lists:nth(Offset+1, Down)).

dr_to_down(Region, DR)
        when Region == <<"EU863-870">>; Region == <<"CN779-787">>; Region == <<"EU433">>;
             Region == <<"CN470-510">>; Region == <<"KR920-923">> ->
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
        when Region == <<"EU863-870">>; Region == <<"CN779-787">>; Region == <<"EU433">>;
             Region == <<"CN470-510">>; Region == <<"KR920-923">> -> [
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
    <<"SF", (integer_to_binary(SF))/binary, "BW", (integer_to_binary(BW))/binary>>.

datar_to_tuple(DataRate) ->
    [SF, BW] = binary:split(DataRate, [<<"SF">>, <<"BW">>], [global, trim_all]),
    {binary_to_integer(SF), binary_to_integer(BW)}.

powers(Region)
        when Region == <<"EU863-870">> -> [
    {0, 20},
    {1, 14},
    {2, 11},
    {3, 8},
    {4, 5},
    {5, 2}];
powers(Region)
        when Region == <<"US902-928">>; Region == <<"US902-928-PR">>;
             Region == <<"AU915-928">> -> [
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
default_adr(<<"KR920-923">>) -> {1, 0, [{0, 2}]}.

% {TXPower, DataRate}
max_adr(<<"EU863-870">>) -> {5, 6};
max_adr(<<"US902-928">>) -> {10, 4};
max_adr(<<"US902-928-PR">>) -> {10, 4};
max_adr(<<"CN779-787">>) -> {5, 6};
max_adr(<<"EU433">>) -> {5, 6};
max_adr(<<"AU915-928">>) -> {10, 4};
max_adr(<<"CN470-510">>) -> {7, 6};
max_adr(<<"KR920-923">>) -> {6, 5}.

% default power for downlinks (dBm)
default_erp(<<"EU863-870">>) -> 14;
default_erp(<<"US902-928">>) -> 20;
default_erp(<<"US902-928-PR">>) -> 20;
default_erp(<<"CN779-787">>) -> 10;
default_erp(<<"EU433">>) -> 10;
default_erp(<<"AU915-928">>) -> 20;
default_erp(<<"CN470-510">>) -> 14;
default_erp(<<"KR920-923">>) -> 14.

% {Min, Max}
freq_range(<<"EU863-870">>) -> {863, 870};
freq_range(<<"US902-928">>) -> {902, 928};
freq_range(<<"US902-928-PR">>) -> {902, 928};
freq_range(<<"CN779-787">>) -> {779, 787};
freq_range(<<"EU433">>) -> {433, 435};
freq_range(<<"AU915-928">>) -> {915, 928};
freq_range(<<"CN470-510">>) -> {470, 510};
freq_range(<<"KR920-923">>) -> {920, 923}.

-include_lib("eunit/include/eunit.hrl").

region_test_()-> [
    ?_assertEqual(dr_to_datar(<<"EU863-870">>, 0), <<"SF12BW125">>),
    ?_assertEqual(dr_to_datar(<<"US902-928">>, 8), <<"SF12BW500">>),
    ?_assertEqual(datar_to_dr(<<"EU863-870">>, <<"SF9BW125">>), 3),
    ?_assertEqual(datar_to_dr(<<"US902-928">>, <<"SF7BW500">>), 13),
    ?_assertEqual(<<"SF10BW500">>, datar_to_down(<<"US902-928">>, <<"SF10BW125">>, 0))].

% end of file
