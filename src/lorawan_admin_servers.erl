%
% Copyright (c) 2016-2019 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_admin_servers).

-export([init/2]).
-export([allowed_methods/2]).
-export([is_authorized/2]).
-export([forbidden/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([resource_exists/2]).
-export([delete_resource/2]).

-export([handle_get/2, handle_write/2, get_server/0]).

-include("lorawan.hrl").
-record(state, {scopes, auth_fields, key}).

init(Req, Scopes) ->
    Key = lorawan_admin:parse_field(sname, cowboy_req:binding(sname, Req)),
    {cowboy_rest, Req, #state{scopes=Scopes, key=Key}}.

allowed_methods(Req, #state{key=undefined}=State) ->
    {[<<"OPTIONS">>, <<"GET">>, <<"POST">>], Req, State};
allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"GET">>, <<"PUT">>, <<"DELETE">>], Req, State}.

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
        {{<<"application">>, <<"json">>, []}, handle_get}
    ], Req, State}.

handle_get(Req, #state{key=undefined}=State) ->
    {jsx:encode([get_server(N) || N <- known_servers()]), Req, State};
handle_get(Req, #state{key=Key}=State) ->
    {jsx:encode(get_server(Key)), Req, State}.

get_server(Node) ->
    case lists:member(Node, nodes([this, connected])) of
        true ->
            rpc:call(Node, ?MODULE, get_server, []);
        false ->
            Config = lorawan_admin:build(?to_map(server, load_server(Node))),
            Config#{
                health_alerts => [<<"disconnected">>],
                health_decay => 100}
    end.

get_server() ->
    Config = lorawan_admin:build(?to_map(server, load_server(node()))),
    Alarms = get_alarms(),
    Config#{
        modules => get_modules(),
        memory => memsup:get_system_memory_data(),
        disk => get_disk_data(),
        health_alerts => Alarms,
        health_decay => length(Alarms)}.

load_server(Node) ->
    case mnesia:dirty_read(server, Node) of
        [S] -> S;
        [] -> #server{sname=Node, router_perf=[]}
    end.

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
        #{sname := SName} ->
            Server = #server{sname=binary_to_atom(SName, latin1)},
            write_server(Req2, Server, State);
        _Else ->
            lager:debug("Bad JSON in HTTP request"),
            {stop, cowboy_req:reply(400, Req2), State}
    end.

write_server(Req, #server{sname=NodeName}=Server, State) ->
    case lorawan_db:join_cluster(node(), NodeName) of
        ok ->
            ok = mnesia:dirty_write(Server),
            {true, Req, State};
        {error, Error} ->
            lager:error("Cannot join node ~p to cluster: ~p", [NodeName, Error]),
            {stop, cowboy_req:reply(400, Req), State}
    end.

resource_exists(Req, #state{key=undefined}=State) ->
    {true, Req, State};
resource_exists(Req, #state{key=Key}=State) ->
    {lists:member(Key, known_servers()), Req, State}.

delete_resource(Req, #state{key=NodeName}=State) ->
    case lorawan_db:leave_cluster(NodeName) of
        ok ->
            ok = mnesia:dirty_delete(server, NodeName),
            {true, Req, State};
        {error, Error} ->
            lager:error("Cannot leave cluster ~p: ~p", [NodeName, Error]),
            {stop, cowboy_req:reply(400, Req), State}
    end.

known_servers() ->
    lists:usort(
        mnesia:dirty_all_keys(server) ++ mnesia:table_info(schema, disc_copies)).

% end of file
