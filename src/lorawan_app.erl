%
% Copyright (c) 2016 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_app).
-behaviour(application).

-export([start/0]).
-export([start/2, stop/1]).

-include("lorawan.hrl").

has_tables(ReqTables) ->
    AllTables = mnesia:system_info(tables),
    lists:all(fun(Table) -> lists:member(Table, AllTables) end, ReqTables).

create_tables() ->
    mnesia:create_table(users, [
        {record_name, user},
        {attributes, record_info(fields, user)},
        {disc_copies, [node()]}]),
    mnesia:create_table(gateways, [
        {record_name, gateway},
        {attributes, record_info(fields, gateway)},
        {disc_copies, [node()]}]),
    mnesia:create_table(devices, [
        {record_name, device},
        {attributes, record_info(fields, device)},
        {disc_copies, [node()]}]),
    mnesia:create_table(links, [
        {record_name, link},
        {attributes, record_info(fields, link)},
        {disc_copies, [node()]}]).

set_defaults() ->
    {ok, {User, Pass}} = application:get_env(lorawan_server, http_admin_credentials),
    mnesia:dirty_write(users, #user{name=User, pass=Pass}).

start() ->
    application:ensure_all_started(lorawan_server).

start(_Type, _Args) ->
    AllTables = [users, gateways, devices, links],
    case has_tables(AllTables) of
        true ->
            mnesia:wait_for_tables(AllTables, 2000);
        false ->
            stopped = mnesia:stop(),
            mnesia:create_schema([node()]),
            ok = mnesia:start(),
            create_tables(),
            mnesia:wait_for_tables(AllTables, 2000),
            set_defaults()
    end,

    lorawan_application:init(),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/applications", lorawan_admin_applications, []},
            {"/applications/:name", lorawan_admin_application, []},
            {"/users", lorawan_admin_users, []},
            {"/users/:name", lorawan_admin_user, []},
            {"/gateways", lorawan_admin_gateways, []},
            {"/gateways/:mac", lorawan_admin_gateway, []},
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
