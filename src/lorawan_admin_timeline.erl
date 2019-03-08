%
% Copyright (c) 2016-2019 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_admin_timeline).

-export([init/2]).
-export([allowed_methods/2]).
-export([is_authorized/2]).
-export([forbidden/2]).
-export([content_types_provided/2]).
-export([resource_exists/2]).

-export([get_timeline/2]).

-include("lorawan.hrl").
-include("lorawan_db.hrl").
-record(state, {scopes, auth_fields}).

init(Req, Scopes) ->
    {cowboy_rest, Req, #state{scopes=Scopes}}.

allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"GET">>], Req, State}.

is_authorized(Req, #state{scopes=Scopes}=State) ->
    case lorawan_admin:handle_authorization(Req, Scopes) of
        {true, AuthFields} ->
            {true, Req, State#state{auth_fields=AuthFields}};
        Else ->
            {Else, Req, State}
    end.

forbidden(Req, #state{auth_fields=AuthFields}=State) ->
    {lorawan_admin:fields_empty(AuthFields), Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, get_timeline}
    ], Req, State}.

get_timeline(Req, State) ->
    #{'start' := Start, 'end' := End} =
        cowboy_req:match_qs([{'start', [], <<>>}, {'end', [], <<>>}], Req),
    Events = lists:map(
        fun (#event{evid=Id, first_rx=Time, last_rx=Time, severity=Severity, text=Text}=Event) ->
                [{id, lorawan_utils:binary_to_hex(Id)},
                    {className, Severity},
                    {content, Text},
                    {title, list_to_binary(title(Event))},
                    {start, Time}];
            (#event{evid=Id, first_rx=StartTime, last_rx=EndTime, severity=Severity, text=Text}=Event) ->
                [{id, lorawan_utils:binary_to_hex(Id)},
                    {className, Severity},
                    {content, Text},
                    {title, list_to_binary(title(Event))},
                    {start, StartTime},
                    {'end', EndTime}]
        end,
        mnesia:dirty_select(event, [{#event{evid='$1', first_rx='$2', last_rx='$3', _='_'},
            select_datetime(Start, End, '$2', '$3'), ['$_']}])),
    RxFrames = lists:map(
        fun(#rxframe{frid=Id, dir=Dir, devaddr=DevAddr, datetime=DateTime, port=Port, data=Data}) ->
            [{id, lorawan_utils:binary_to_hex(Id)},
                {className,
                    case Data of
                        undefined -> <<"info">>;
                        _Else -> Dir
                    end},
                {content, addr_port(DevAddr, Port)},
                {start, DateTime}]
        end,
        mnesia:dirty_select(rxframe, [{#rxframe{datetime='$1', _='_'},
            select_datetime(Start, End, '$1', '$1'), ['$_']}])),
    {jsx:encode([{items, Events++RxFrames}]), Req, State}.

addr_port(DevAddr, undefined) ->
    lorawan_utils:binary_to_hex(DevAddr);
addr_port(DevAddr, Port) ->
    <<(lorawan_utils:binary_to_hex(DevAddr))/binary, ":", (integer_to_binary(Port))/binary>>.

select_datetime(<<>>, <<>>, _, _) ->
    [];
select_datetime(WStart, <<>>, _, EEnd) ->
    [{'>=', EEnd, {const, iso8601:parse(WStart)}}];
select_datetime(<<>>, WEnd, EStart, _) ->
    [{'=<', EStart, {const, iso8601:parse(WEnd)}}];
select_datetime(WStart, WEnd, EStart, EEnd) ->
    [{'>=', EEnd, {const, iso8601:parse(WStart)}}, {'=<', EStart, {const, iso8601:parse(WEnd)}}].

title(#event{entity=Entity, eid=undefined}=Event) ->
    [io_lib:print(Entity), "<br\>", title0(Event)];
title(#event{entity=Entity, eid=EID}=Event) when is_atom(EID) ->
    [io_lib:print(Entity), " ", atom_to_list(EID), "<br\>", title0(Event)];
title(#event{entity=Entity, eid=EID}=Event) ->
    [io_lib:print(Entity), " ", lorawan_utils:binary_to_hex(EID), "<br\>", title0(Event)].

title0(#event{text=Text, args=undefined}) ->
    Text;
title0(#event{text=Text, args=Args}) ->
    [Text, " ", Args].

resource_exists(Req, State) ->
    {true, Req, State}.

% end of file
