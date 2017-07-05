%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_utils).

-export([throw_info/2, throw_warning/2, throw_error/2]).

-include("lorawan.hrl").

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

% end of file
