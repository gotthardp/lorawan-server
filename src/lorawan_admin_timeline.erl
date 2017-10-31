%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_admin_timeline).

-export([init/2]).
-export([is_authorized/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([resource_exists/2]).

-export([get_timeline/2]).

-include_lib("lorawan_server_api/include/lorawan_application.hrl").
-include("lorawan.hrl").

init(Req, []) ->
    {cowboy_rest, Req, undefined}.

is_authorized(Req, State) ->
    lorawan_admin:handle_authorization(Req, State).

allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, get_timeline}
    ], Req, State}.

get_timeline(Req, State) ->
    #{'start' := Start, 'end' := End} =
        cowboy_req:match_qs([{'start', [], <<>>}, {'end', [], <<>>}], Req),
    Events = lists:map(
        fun (#event{evid=Id, first_rx=Time, last_rx=Time, severity=Severity, text=Text}=Event) ->
                [{id, lorawan_mac:binary_to_hex(Id)},
                    {className, Severity},
                    {content, Text},
                    {title, list_to_binary(title(Event))},
                    {start, Time}];
            (#event{evid=Id, first_rx=StartTime, last_rx=EndTime, severity=Severity, text=Text}=Event) ->
                [{id, lorawan_mac:binary_to_hex(Id)},
                    {className, Severity},
                    {content, Text},
                    {title, list_to_binary(title(Event))},
                    {start, StartTime},
                    {'end', EndTime}]
        end,
        mnesia:dirty_select(events, [{#event{evid='$1', first_rx='$2', last_rx='$3', _='_'},
            select_datetime(Start, End, '$2', '$3'), ['$_']}])),
    RxFrames = lists:map(
        fun({Id, DevAddr, DateTime, Port, Data}) ->
            [{id, lorawan_mac:binary_to_hex(Id)},
                {className,
                    case Data of
                        undefined -> <<"info">>;
                        _Else -> <<"node">>
                    end},
                {content, addr_port(DevAddr, Port)},
                {start, DateTime}]
        end,
        mnesia:dirty_select(rxframes, [{#rxframe{frid='$1', devaddr='$2', datetime='$3', port='$4', data='$5', _='_'},
            select_datetime(Start, End, '$3', '$3'), [{{'$1', '$2', '$3', '$4', '$5'}}]}])),
    {jsx:encode([{items, Events++RxFrames}]), Req, State}.

addr_port(DevAddr, undefined) ->
    lorawan_mac:binary_to_hex(DevAddr);
addr_port(DevAddr, Port) ->
    <<(lorawan_mac:binary_to_hex(DevAddr))/binary, ":", (integer_to_binary(Port))/binary>>.

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
title(#event{entity=Entity, eid=EID}=Event) ->
    [io_lib:print(Entity), " ", lorawan_mac:binary_to_hex(EID), "<br\>", title0(Event)].

title0(#event{text=Text, args=undefined}) ->
    Text;
title0(#event{text=Text, args=Args}) ->
    [Text, " ", Args].

resource_exists(Req, State) ->
    {true, Req, State}.

% end of file
