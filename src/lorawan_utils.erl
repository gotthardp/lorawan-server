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


throw_event(Severity, Entity, undefined, Message) ->
    lager:log(Severity, self(), "~s ~w", [Entity, Message]),
    write_event(Severity, Entity, undefined, Message);

throw_event(Severity, Entity, EID, Message) ->
    lager:log(Severity, self(), "~s ~s ~w", [Entity, lorawan_mac:binary_to_hex(EID), Message]),
    write_event(Severity, Entity, EID, Message).

write_event(Severity, Entity, EID, Message) ->
    Text = list_to_binary(io_lib:write(Message)),
    EvId = crypto:hash(md4, term_to_binary({Entity, EID, Text})),
    mnesia:transaction(fun() ->
        case mnesia:read(events, EvId, write) of
            [E] ->
                mnesia:write(events, E#event{count=inc(E#event.count)}, write);
            [] ->
                mnesia:write(events, #event{evid=EvId, severity=Severity, count=1,
                    datetime=calendar:universal_time(), entity=Entity, eid=EID, text=Text}, write)
        end
    end).

inc(undefined) -> 1;
inc(Num) -> Num+1.

% end of file
