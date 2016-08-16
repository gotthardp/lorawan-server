%
% Copyright (c) 2016 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-include("lorawan.hrl").

has_tables(ReqTables) ->
    AllTables = mnesia:system_info(tables),
    lists:all(fun(Table) -> lists:member(Table, AllTables) end, ReqTables).

create_tables() ->
    mnesia:create_table(devices, [
        {record_name, device},
        {attributes, record_info(fields, device)},
        {disc_copies, [node()]}]),
    mnesia:create_table(links, [
        {record_name, link},
        {attributes, record_info(fields, link)},
        {disc_copies, [node()]}]).

start(_Type, _Args) ->
    case has_tables([devices, links]) of
        true -> ok;
        false ->
            stopped = mnesia:stop(),
            mnesia:create_schema([node()]),
            ok = mnesia:start(),
            create_tables()
    end,

    lorawan_application:init(),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/applications", lorawan_admin_applications, []},
            {"/devices", lorawan_admin_devices, []},
            {"/devices/:deveui", lorawan_admin_device, []},
            {"/links", lorawan_admin_links, []},
            {"/links/:devaddr", lorawan_admin_link, []},
            {"/admin", cowboy_static, {priv_file, lorawan_server, "admin/index.html"}},
            {"/admin/[...]", cowboy_static, {priv_dir, lorawan_server, "admin"}}
        ]}
    ]),
    {ok, Port} = application:get_env(http_admin_port),
    {ok, _} = cowboy:start_clear(http, 100, [{port, Port}], #{
        env => #{dispatch => Dispatch}
    }),
    lorawan_sup:start_link().

stop(_State) ->
    ok.

% end of file
