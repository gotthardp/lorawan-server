%
% Copyright (c) 2016 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_app).
-behaviour(application).

-export([start/0]).
-export([start/2, stop/1]).

start() ->
    {ok, _Started} = application:ensure_all_started(lorawan_server).

start(_Type, _Args) ->
    lorawan_db:ensure_tables(),
    {ok, _} = timer:apply_interval(3600*1000, lorawan_db, trim_tables, []),

    {ok, Handlers} = lorawan_application:init(),
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
            {"/txframes", lorawan_admin_txframes, []},
            {"/txframes/:frid", lorawan_admin_txframe, []},
            {"/rxq/:devaddr", lorawan_admin_rxq, []},
            {"/", cowboy_static, {priv_file, lorawan_server, "root.html"}},
            {"/admin", cowboy_static, {priv_file, lorawan_server, "admin/index.html"}},
            {"/admin/[...]", cowboy_static, {priv_dir, lorawan_server, "admin"}}
        ]++Handlers}
    ]),
    {ok, Port} = application:get_env(http_admin_port),
    {ok, _} = cowboy:start_clear(http, 100, [{port, Port}], #{
        env => #{dispatch => Dispatch}
    }),
    lorawan_sup:start_link().

stop(_State) ->
    ok.

% end of file
