%
% Copyright (c) 2016-2018 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_admin_servers).

-export([init/2]).
-export([is_authorized/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([resource_exists/2]).

-export([handle_get/2, handle_write/2]).

-include("lorawan.hrl").
-record(state, {key}).

init(Req, _Opts) ->
    Key = cowboy_req:binding(name, Req),
    {cowboy_rest, Req, #state{key=Key}}.

is_authorized(Req, State) ->
    lorawan_admin:handle_authorization(Req, State).

allowed_methods(Req, #state{key=undefined}=State) ->
    {[<<"OPTIONS">>, <<"GET">>], Req, State};
allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"GET">>, <<"PUT">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, handle_get}
    ], Req, State}.

handle_get(Req, #state{key=undefined}=State) ->
    {jsx:encode([get_server()]), Req, State};
handle_get(Req, State) ->
    {jsx:encode(get_server()), Req, State}.

get_server() ->
    Server =
        case mnesia:dirty_read(servers, node()) of
            [S] -> S;
            [] -> #server{sname=node(), router_perf=[]}
        end,
    Config = lorawan_admin:build(?to_map(server, Server)),
    Alarms = get_alarms(),
    Config#{
        modules => get_modules(),
        memory => memsup:get_system_memory_data(),
        disk => get_disk_data(),
        health_alerts => Alarms,
        health_decay => length(Alarms)}.

get_modules() ->
    lists:map(
        fun({App, _Desc, Vsn}) ->
            {App, list_to_binary(Vsn)}
        end,
        application:which_applications()).

get_disk_data() ->
    lists:map(
        fun({Id, KByte, Capacity}) ->
            [{id, list_to_binary(Id)}, {size_kb, KByte}, {percent_used, Capacity}]
        end,
        disksup:get_disk_data()).

get_alarms() ->
    lists:map(
        fun
            ({Id, _Desc}) when is_atom(Id) -> Id;
            ({{Id, _}, _Desc}) when is_atom(Id) -> Id
        end,
        alarm_handler:get_alarms()).

content_types_accepted(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, '*'}, handle_write}
    ], Req, State}.

handle_write(Req, State) ->
    {ok, Data, Req2} = cowboy_req:read_body(Req),
    case catch jsx:decode(Data, [return_maps, {labels, atom}]) of
        Struct when is_map(Struct) ->
            ok = mnesia:dirty_write(servers, ?to_record(server, lorawan_admin:parse(Struct), undefined)),
            {true, Req2, State};
        _Else ->
            lager:debug("Bad JSON in HTTP request"),
            {stop, cowboy_req:reply(400, Req2), State}
    end.

resource_exists(Req, #state{key=undefined}=State) ->
    {true, Req, State};
resource_exists(Req, #state{key=Key}=State) ->
    case atom_to_binary(node(), latin1) of
        Key ->
            {true, Req, State};
        _Else ->
            {false, Req, State}
    end.

% end of file
