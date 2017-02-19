%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_app).
-behaviour(application).

-export([start/0]).
-export([start/2, stop/1]).

-include_lib("lorawan_server_api/include/lorawan_application.hrl").
-include("lorawan.hrl").

start() ->
    {ok, _Started} = application:ensure_all_started(lorawan_server).

start(_Type, _Args) ->
    lorawan_db:ensure_tables(),
    {ok, _} = timer:apply_interval(3600*1000, lorawan_db, trim_tables, []),

    {ok, Handlers} = lorawan_application_handler:init(),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/applications/[:name]", lorawan_admin_applications, []},
            {"/users/[:name]", lorawan_admin_database,
                [users, user, record_info(fields, user), text]},
            {"/gateways/[:mac]", lorawan_admin_database,
                [gateways, gateway, record_info(fields, gateway), binary]},
            {"/devices/[:deveui]", lorawan_admin_database,
                [devices, device, record_info(fields, device), binary]},
            {"/links/[:devaddr]", lorawan_admin_database,
                [links, link, record_info(fields, link), binary]},
            {"/ignored_links/[:devaddr]", lorawan_admin_database,
                [ignored_links, ignored_link, record_info(fields, ignored_link), binary]},
            {"/txframes/[:frid]", lorawan_admin_database,
                [txframes, txframe, record_info(fields, txframe), binary]},
            {"/rxframes/[:frid]", lorawan_admin_database,
                [rxframes, rxframe, record_info(fields, rxframe), binary]},
            {"/rgraph/:devaddr", lorawan_admin_rxgraph, [rgraph]},
            {"/qgraph/:devaddr", lorawan_admin_rxgraph, [qgraph]},
            {"/devstat/:devaddr", lorawan_admin_devstat, []},
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
