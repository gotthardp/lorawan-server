%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_utils).

-export([index_of/2]).
-export([precise_universal_time/0, ms_diff/2, datetime_to_timestamp/1, apply_offset/2]).
-export([throw_info/2, throw_warning/2, throw_error/2]).

-include("lorawan.hrl").

-define(MEGA, 1000000).

index_of(Item, List) -> index_of(Item, List, 1).

index_of(_, [], _)  -> undefined;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).


ms_diff({MSecs1, Secs1, USecs1}, {MSecs2, Secs2, USecs2}) when MSecs1 =< MSecs2 ->
    1000*(?MEGA*(MSecs2-MSecs1)+(Secs2-Secs1))
        +(USecs2-USecs1) div 1000.

precise_universal_time() ->
    {Date, {Hours, Min, Secs}} = calendar:universal_time(),
    {_, _, USecs} = erlang:timestamp(),
    {Date, {Hours, Min, Secs + (USecs div 1000)/1000}}.

datetime_to_timestamp({Date, {Hours, Min, Secs}}) ->
    TotalSecs =
        calendar:datetime_to_gregorian_seconds({Date, {Hours, Min, trunc(Secs)}})
        - epoch_seconds(),
    {TotalSecs div ?MEGA, TotalSecs rem ?MEGA, trunc(?MEGA*Secs)-?MEGA*trunc(Secs)};
datetime_to_timestamp(undefined) ->
    {0, 0, 0}. %% midnight

epoch_seconds() ->
    calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}).

apply_offset({Date, {Hours, Min, Secs}}, {OHours, OMin, OSecs}) ->
    TotalSecs =
        calendar:datetime_to_gregorian_seconds({Date, {Hours, Min, trunc(Secs)}})
        + (60*((60*OHours) + OMin)) + OSecs,
    {Date2, {Hours2, Min2, Secs2}} = calendar:gregorian_seconds_to_datetime(TotalSecs),
    {Date2, {Hours2, Min2, Secs2+(Secs-trunc(Secs))}}.

throw_info({Entity, EID}, Text) ->
    throw_event(info, Entity, EID, Text);
throw_info(Entity, Text) ->
    throw_event(info, Entity, undefined, Text).

throw_warning({Entity, EID}, Text) ->
    throw_event(warning, Entity, EID, Text);
throw_warning(Entity, Text) ->
    throw_event(warning, Entity, undefined, Text).

throw_error({Entity, EID}, Text) ->
    throw_event(error, Entity, EID, Text);
throw_error(Entity, Text) ->
    throw_event(error, Entity, undefined, Text).


throw_event(Severity, Entity, undefined, Event) ->
    lager:log(Severity, self(), "~s ~p", [Entity, Event]),
    write_event(Severity, Entity, undefined, Event);

throw_event(Severity, Entity, EID, Event) ->
    lager:log(Severity, self(), "~s ~s ~p", [Entity, lorawan_mac:binary_to_hex(EID), Event]),
    write_event(Severity, Entity, EID, Event).

write_event(Severity, Entity, EID, Event) ->
    Text = list_to_binary(io_lib:print(Event)),
    EvId = crypto:hash(md4, term_to_binary({Entity, EID,
        case Event of
            {First, _} -> First;
            Only -> Only
        end})),
    {atomic, ok} =
        mnesia:transaction(fun() ->
            case mnesia:read(events, EvId, write) of
                [E] ->
                    mnesia:write(events, E#event{last_rx=calendar:universal_time(),
                        count=inc(E#event.count), text=Text}, write);
                [] ->
                    mnesia:write(events, #event{evid=EvId, severity=Severity,
                        first_rx=calendar:universal_time(), last_rx=calendar:universal_time(),
                        count=1, entity=Entity, eid=EID, text=Text}, write)
            end
        end),
    ok.

inc(undefined) -> 1;
inc(Num) -> Num+1.

-include_lib("eunit/include/eunit.hrl").

time_test_()-> [
    ?_assertEqual({0,1,0}, datetime_to_timestamp({{1970,1,1}, {0,0,1}})),
    ?_assertEqual({0,10,1000}, datetime_to_timestamp({{1970,1,1}, {0,0,10.001}})),
    ?_assertEqual(1900, ms_diff(datetime_to_timestamp({{2017,1,1}, {13,0,1.1}}), datetime_to_timestamp({{2017,1,1}, {13,0,3}}))),
    ?_assertEqual(1, ms_diff(datetime_to_timestamp({{2017,1,1}, {13,1,59.999}}), datetime_to_timestamp({{2017,1,1}, {13,2,0}}))),
    ?_assertEqual({{1989,11,17}, {16,59,10.001}}, apply_offset({{1989,11,17}, {18,0,10.001}}, {-1,-1,0}))
].

% end of file
