%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_mac_commands).

-export([handle/2]).

-include_lib("lorawan_server_api/include/lorawan_application.hrl").

handle(Link, FOpts) ->
    % process incoming commands
    FOptsIn = parse_fopts(FOpts),
    case FOptsIn of
        [] -> ok;
        List1 -> lager:debug("~w -> ~w", [Link#link.devaddr, List1])
    end,
    Link2 = lists:foldl(
        fun(FOpt, L) -> handle_fopt(FOpt, L) end,
        Link, FOptsIn),
    % check for new commands
    ResA = send_adr({Link2, []}),
    {Link3, FOptsOut} = request_status(ResA),
    case FOptsOut of
        [] -> ok;
        List2 -> lager:debug("~w <- ~w", [Link#link.devaddr, List2])
    end,
    {ok, Link3, encode_fopts(FOptsOut)}.


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
    lager:warning("Unknown command ~w", [Unknown]),
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


handle_fopt({link_adr_ans,1,1,1}, Link) ->
    lager:debug("LinkADRReq succeeded"),
    Link#link{adr_use=Link#link.adr_set};
handle_fopt({link_adr_ans, PowerACK, DataRateACK, ChannelMaskACK}, Link) ->
    lager:warning("LinkADRReq failed: power ~B, datr ~B, chans ~B", [PowerACK, DataRateACK, ChannelMaskACK]),
    {TXPower, DataRate, Chans} = Link#link.adr_set,
    % clear the settings that failed
    Link#link{adr_set = {clear_when_zero(PowerACK, TXPower), clear_when_zero(DataRateACK, DataRate),
        clear_when_zero(ChannelMaskACK, Chans)}};
handle_fopt({dev_status_ans, Battery, Margin}, Link) ->
    lager:debug("DevStatus: battery ~B, margin: ~B", [Battery, Margin-32]),
    Link#link{devstat_time=calendar:universal_time(), devstat_fcnt=Link#link.fcntup, devstat={Battery, Margin-32}};
handle_fopt(Unknown, Link) ->
    lager:debug("Unknown FOpt ~w", [Unknown]),
    Link.


send_adr({Link, FOptsOut}) ->
    IsIncomplete = case Link#link.adr_set of
        undefined -> true;
        Tuple -> lists:member(undefined, tuple_to_list(Tuple))
    end,
    if
        Link#link.adr_flag_use == 1, Link#link.adr_flag_set == 1,
        not IsIncomplete, Link#link.adr_use /= Link#link.adr_set ->
            lager:debug("LinkADRReq ~w", [Link#link.adr_set]),
            {Link, set_channels(Link#link.region, Link#link.adr_set, FOptsOut)};
        true ->
            {Link, FOptsOut}
    end.

set_channels(Region, {TXPower, DataRate, Chans}, FOptsOut)
        when Region == <<"EU863-870">>; Region == <<"CN779-787">>; Region == <<"EU433">> ->
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

request_status({#link{devstat_time=LastDate, devstat_fcnt=LastFCnt}=Link, FOptsOut})
        when LastDate == undefined; LastFCnt == undefined ->
    {Link, [dev_status_req | FOptsOut]};
request_status({#link{devstat_time=LastDate, devstat_fcnt=LastFCnt}=Link, FOptsOut}) ->
    {ok, {MaxTime, MaxFCnt}} = application:get_env(devstat_gap),
    TimeDiff = calendar:datetime_to_gregorian_seconds(calendar:universal_time())
                - calendar:datetime_to_gregorian_seconds(LastDate),
    if
        TimeDiff > MaxTime;
        Link#link.fcntup - LastFCnt > MaxFCnt ->
            {Link, [dev_status_req | FOptsOut]};
        true ->
            {Link, FOptsOut}
    end.

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
