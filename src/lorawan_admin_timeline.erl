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
        fun({Id, Text, DateTime}) ->
            [{id, lorawan_mac:binary_to_hex(Id)},
                {group, <<"node">>},
                {content, Text},
                {start, DateTime}]
        end,
        mnesia:dirty_select(events, [{#event{evid='$1', text='$2', last_rx='$3', _='_'},
            select_datetime(Start, End), [{{'$1', '$2', '$3'}}]}])),
    RxFrames = lists:map(
        fun({Id, DevAddr, DateTime}) ->
            [{id, lorawan_mac:binary_to_hex(Id)},
                {group, <<"node">>},
                {content, lorawan_mac:binary_to_hex(DevAddr)},
                {start, DateTime}]
        end,
        mnesia:dirty_select(rxframes, [{#rxframe{frid='$1', devaddr='$2', datetime='$3', _='_'},
            select_datetime(Start, End), [{{'$1', '$2', '$3'}}]}])),
    {jsx:encode([{items, Events++RxFrames}]), Req, State}.

select_datetime(<<>>, <<>>) ->
    [];
select_datetime(Start, <<>>) ->
    [{'>=', '$3', {const, iso8601:parse(Start)}}];
select_datetime(<<>>, End) ->
    [{'=<', '$3', {const, iso8601:parse(End)}}];
select_datetime(Start, End) ->
    [{'>=', '$3', {const, iso8601:parse(Start)}}, {'=<', '$3', {const, iso8601:parse(End)}}].

resource_exists(Req, State) ->
    {true, Req, State}.

% end of file
