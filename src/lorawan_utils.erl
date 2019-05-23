%
% Copyright (c) 2016-2019 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_utils).

-export([binary_to_hex/1, hex_to_binary/1, reverse/1]).
-export([index_of/2]).
-export([precise_universal_time/0, time_to_gps/0, time_to_gps/1, time_to_unix/0, time_to_unix/1]).
-export([ms_diff/2, datetime_to_timestamp/1, apply_offset/2]).
-export([throw_info/2, throw_info/3, throw_warning/2, throw_warning/3, throw_error/2, throw_error/3]).

-include("lorawan.hrl").

-define(MEGA, 1000000).

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

reverse(Bin) -> reverse(Bin, <<>>).
reverse(<<>>, Acc) -> Acc;
reverse(<<H:1/binary, Rest/binary>>, Acc) ->
    reverse(Rest, <<H/binary, Acc/binary>>).


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

time_to_gps() ->
    time_to_gps(precise_universal_time()).

time_to_gps({Date, {Hours, Min, Secs}}) ->
    TotalSecs = calendar:datetime_to_gregorian_seconds({Date, {Hours, Min, trunc(Secs)}})
            - calendar:datetime_to_gregorian_seconds({{1980, 1, 6}, {0, 0, 0}})
            + 17, % leap seconds
    trunc(1000*(TotalSecs + (Secs - trunc(Secs)))). % ms

time_to_unix() ->
    time_to_gps(precise_universal_time()).

time_to_unix({Date, {Hours, Min, Secs}}) ->
    TotalSecs = calendar:datetime_to_gregorian_seconds({Date, {Hours, Min, trunc(Secs)}})
            - epoch_seconds(),
    trunc(1000*(TotalSecs + (Secs - trunc(Secs)))). % ms

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

throw_info(Entity, Text) ->
    throw_info(Entity, Text, unique).

throw_info({Entity, EID}, Text, Mark) ->
    throw_event(info, {Entity, EID}, Text, Mark);
throw_info(Entity, Text, Mark) ->
    throw_event(info, {Entity, undefined}, Text, Mark).

throw_warning(Entity, Text) ->
    throw_warning(Entity, Text, unique).

throw_warning({Entity, EID}, Text, Mark) ->
    throw_event(warning, {Entity, EID}, Text, Mark);
throw_warning(Entity, Text, Mark) ->
    throw_event(warning, {Entity, undefined}, Text, Mark).

throw_error(Entity, Text) ->
    throw_error(Entity, Text, unique).

throw_error({Entity, EID}, Text, Mark) ->
    throw_event(error, {Entity, EID}, Text, Mark);
throw_error(Entity, Text, Mark) ->
    throw_event(error, {Entity, undefined}, Text, Mark).


throw_event(Severity, {Entity, undefined}, Text, Mark) ->
    lager:log(Severity, self(), "~s ~p", [Entity, Text]),
    write_event(Severity, {Entity, undefined}, Text, Mark);

throw_event(Severity, {Entity, EID}, Text, Mark) ->
    if
        Entity == server; Entity == connector ->
            lager:log(Severity, self(), "~s ~s ~p", [Entity, EID, Text]);
        true ->
            lager:log(Severity, self(), "~s ~s ~p", [Entity, lorawan_utils:binary_to_hex(EID), Text])
    end,
    write_event(Severity, {Entity, EID}, Text, Mark).

write_event(Severity, {Entity, EID}, Text, unique) ->
    % first_rx and last_rx shall be identical
    Time = calendar:universal_time(),
    {Event, Args} = event_args(Text),
    EvId = evid({Entity, EID}, Event, Time),
    mnesia:dirty_write(event, #event{evid=EvId, severity=Severity,
        first_rx=Time, last_rx=Time, count=1, entity=Entity, eid=EID, text=Event, args=Args});
write_event(Severity, {Entity, EID}, Text, Mark) ->
    {Event, Args} = event_args(Text),
    EvId = evid({Entity, EID}, Event, Mark),
    {atomic, ok} =
        mnesia:transaction(fun() ->
            case mnesia:read(event, EvId, write) of
                [E] ->
                    mnesia:write(E#event{last_rx=calendar:universal_time(),
                        count=inc(E#event.count), text=Event, args=Args});
                [] ->
                    % first_rx and last_rx shall be identical
                    Time = calendar:universal_time(),
                    mnesia:write(#event{evid=EvId, severity=Severity,
                        first_rx=Time, last_rx=Time, count=1,
                        entity=Entity, eid=EID, text=Event, args=Args})
            end
        end),
    ok.

evid(EntityID, Event, Mark) ->
    crypto:hash(md4, term_to_binary({EntityID, Event, Mark})).

event_args({Event, Args}) ->
    {atom_to_binary(Event, latin1), list_to_binary(io_lib:print(Args))};
event_args(Event) when is_atom(Event) ->
    {atom_to_binary(Event, latin1), undefined};
% for example gateway errors are sent as binaries
event_args(Event) when is_binary(Event) ->
    {Event, undefined}.

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
