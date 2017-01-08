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
    Link2 = lists:foldl(fun(FOpt, L) -> handle_fopt(FOpt, L) end,
        Link, parse_fopts(FOpts)),
    % check for new commands
    ResA = send_adr({Link2, []}),
    {Link3, FOptsOut} = request_status(ResA),
    {ok, Link3, encode_fopts(FOptsOut)}.


parse_fopts(FOpts) ->
    parse_fopt(FOpts, []).

parse_fopt(<<16#02, Rest/binary>>, Acc) ->
    parse_fopt(Rest, [link_check_req | Acc]);
parse_fopt(<<16#03, _RFU:5, PowerACK:1, DataRateACK:1, ChannelMaskACK:1, Rest/binary>>, Acc) ->
    parse_fopt(Rest, [{link_adr_ans, PowerACK, DataRateACK, ChannelMaskACK} | Acc]);
parse_fopt(<<16#04, Rest/binary>>, Acc) ->
    parse_fopt(Rest, [duty_cycle_ans | Acc]);
parse_fopt(<<16#05, _RFU:5, RX1DROffsetACK:1, RX2DataRateACK:1, ChannelACK:1, Rest/binary>>, Acc) ->
    parse_fopt(Rest, [{rx_param_setup_ans, RX1DROffsetACK, RX2DataRateACK, ChannelACK} | Acc]);
parse_fopt(<<16#06, Battery:8, _RFU:2, Margin:6, Rest/binary>>, Acc) ->
    parse_fopt(Rest, [{dev_status_ans, Battery, Margin} | Acc]);
parse_fopt(<<16#07, _RFU:6, DataRateRangeOK:1, ChannelFreqOK:1, Rest/binary>>, Acc) ->
    parse_fopt(Rest, [{new_channel_ans, DataRateRangeOK, ChannelFreqOK} | Acc]);
parse_fopt(<<16#08, Rest/binary>>, Acc) ->
    parse_fopt(Rest, [rx_timing_setup_ans | Acc]);
parse_fopt(<<>>, Acc) ->
    Acc;
parse_fopt(Unknown, Acc) ->
    lager:warning("Unknown command ~w", [Unknown]),
    Acc.


encode_fopts(FOpts) ->
    encode_fopt(FOpts, <<>>).

encode_fopt([{link_check_ans, Margin, GwCnt} | Rest], Acc) ->
    encode_fopt(Rest, <<16#02, Margin, GwCnt, Acc/binary>>);
encode_fopt([{link_adr_req, DataRate, TXPower, ChMask, ChMaskCntl, NbRep} | Rest], Acc) ->
    encode_fopt(Rest, <<16#03, DataRate:4, TXPower:4, ChMask:16/little-unsigned-integer, 0:1, ChMaskCntl:3, NbRep:4, Acc/binary>>);
encode_fopt([{duty_cycle_req, MaxDCycle} | Rest], Acc) ->
    encode_fopt(Rest, <<16#04, MaxDCycle, Acc/binary>>);
encode_fopt([{rx_param_setup_req, RX1DROffset, RX2DataRate, Frequency} | Rest], Acc) ->
    encode_fopt(Rest, <<16#05, 0:1, RX1DROffset:3, RX2DataRate:4, Frequency:24/little-unsigned-integer, Acc/binary>>);
encode_fopt([dev_status_req | Rest], Acc) ->
    encode_fopt(Rest, <<16#06, Acc/binary>>);
encode_fopt([{new_channel_req, ChIndex, Freq, MaxDR, MinDR} | Rest], Acc) ->
    encode_fopt(Rest, <<16#07, ChIndex, Freq:24/little-unsigned-integer, MaxDR:4, MinDR:4, Acc/binary>>);
encode_fopt([{rx_timing_setup_req, Delay} | Rest], Acc) ->
    encode_fopt(Rest, <<16#08, 0:4, Delay:4, Acc/binary>>);
encode_fopt([], Acc) ->
    Acc.


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
    lager:debug("DevStatus: battry ~B, margin: ~B", [Battery, Margin]),
    Link#link{devstat_time=calendar:universal_time(), devstat_fcnt=Link#link.fcntup, devstat={Battery, Margin}};
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
            {TXPower, DataRate, Chans} = Link#link.adr_set,
            {Link, [{link_adr_req, DataRate, TXPower, Chans, 0, 0} | FOptsOut]};
        true ->
            {Link, FOptsOut}
    end.

request_status({#link{devstat_time=LastDate, devstat_fcnt=LastFCnt}=Link, FOptsOut})
        when LastDate == undefined; LastFCnt == undefined ->
    lager:debug("DevStatusReq"),
    {Link, [dev_status_req | FOptsOut]};
request_status({#link{devstat_time=LastDate, devstat_fcnt=LastFCnt}=Link, FOptsOut}) ->
    {ok, {MaxTime, MaxFCnt}} = application:get_env(devstat_gap),
    TimeDiff = calendar:datetime_to_gregorian_seconds(calendar:universal_time())
                - calendar:datetime_to_gregorian_seconds(LastDate),
    if
        TimeDiff > MaxTime;
        Link#link.fcntup - LastFCnt > MaxFCnt ->
            lager:debug("DevStatusReq"),
            {Link, [dev_status_req | FOptsOut]};
        true ->
            {Link, FOptsOut}
    end.


clear_when_zero(0, _Value) -> undefined;
clear_when_zero(_Else, Value) -> Value.

% end of file
