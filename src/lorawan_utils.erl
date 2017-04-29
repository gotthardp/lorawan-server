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


throw_event(Severity, Entity, undefined, Text) ->
    lager:log(Severity, self(), "~s ~w", [Entity, Text]),
    write_event(Severity, Entity, undefined, Text);

throw_event(Severity, Entity, EID, Text) ->
    lager:log(Severity, self(), "~s ~s ~w", [Entity, lorawan_mac:binary_to_hex(EID), Text]),
    write_event(Severity, Entity, EID, Text).

write_event(Severity, Entity, EID, Text) ->
    mnesia:dirty_write(events, #event{evid= <<(erlang:system_time()):64>>,
        severity=Severity, datetime=calendar:universal_time(),
        entity=Entity, eid=EID, text=list_to_binary(io_lib:write(Text))}).

% end of file
