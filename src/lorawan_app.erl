%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_app).
-behaviour(application).

-export([start/0]).
-export([start/2, stop/1]).

-include("lorawan_application.hrl").
-include("lorawan.hrl").

start() ->
    {ok, _Started} = application:ensure_all_started(lorawan_server).

start(_Type, _Args) ->
    ok = ensure_erlang_version(19),
    lorawan_db:ensure_tables(),
    syn:init(),

    {ok, Handlers} = lorawan_handler:init(),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/servers", lorawan_admin_servers, []},
            {"/applications/[:name]", lorawan_admin_applications, []},
            {"/users/[:name]", lorawan_admin_db_record,
                [users, user, record_info(fields, user)]},
            {"/networks/[:name]", lorawan_admin_db_record,
                [networks, network, record_info(fields, network)]},
            {"/gateways/[:mac]", lorawan_admin_db_record,
                [gateways, gateway, record_info(fields, gateway)]},
            {"/multicast_channels/[:devaddr]", lorawan_admin_db_record,
                [multicast_channels, multicast_channel, record_info(fields, multicast_channel)]},
            {"/profiles/[:name]", lorawan_admin_db_record,
                [profiles, profile, record_info(fields, profile)]},
            {"/devices/[:deveui]", lorawan_admin_db_record,
                [devices, device, record_info(fields, device)]},
            {"/nodes/[:devaddr]", lorawan_admin_db_record,
                [nodes, node, record_info(fields, node)]},
            {"/ignored_nodes/[:devaddr]", lorawan_admin_db_record,
                [ignored_nodes, ignored_node, record_info(fields, ignored_node)]},
            {"/txframes/[:frid]", lorawan_admin_db_record,
                [txframes, txframe, record_info(fields, txframe)]},
            {"/rxframes/[:frid]", lorawan_admin_db_record,
                [rxframes, rxframe, record_info(fields, rxframe)]},
            {"/handlers/[:appid]", lorawan_admin_db_record,
                [handlers, handler, record_info(fields, handler)]},
            {"/connectors/[:connid]", lorawan_admin_db_record,
                [connectors, connector, record_info(fields, connector)]},
            {"/events/[:evid]", lorawan_admin_db_record,
                [events, event, record_info(fields, event)]},
            {"/upload", lorawan_admin_upload, []},
            {"/timeline", lorawan_admin_timeline, []},
            {"/pgraph/:mac", lorawan_admin_gwgraph, [pgraph]},
            {"/tgraph/:mac", lorawan_admin_gwgraph, [tgraph]},
            {"/rgraph/:devaddr", lorawan_admin_rxgraph, [rgraph]},
            {"/qgraph/:devaddr", lorawan_admin_rxgraph, [qgraph]},
            {"/devstat/:devaddr", lorawan_admin_devstat, []},
            {"/", cowboy_static, {priv_file, lorawan_server, "root.html"}},
            {"/favicon.ico", cowboy_static, {priv_file, lorawan_server, "favicon.ico"}},
            {"/admin", cowboy_static, {priv_file, lorawan_server, "admin/index.html"}},
            {"/admin/[...]", cowboy_static, {priv_dir, lorawan_server, "admin"}}
        ]++Handlers}
    ]),
    case application:get_env(http_admin_listen) of
        undefined ->
            ok;
        {ok, undefined} ->
            ok;
        {ok, HttpOpts} ->
            {ok, _} = cowboy:start_clear(http, HttpOpts,
                #{env => #{dispatch => Dispatch},
                stream_handlers => [lorawan_admin_logger, cowboy_compress_h, cowboy_stream_h]})
    end,
    case application:get_env(http_admin_listen_ssl) of
        undefined ->
            ok;
        {ok, undefined} ->
            ok;
        {ok, SslOpts} ->
            {ok, _} = cowboy:start_tls(https, SslOpts,
                #{env => #{dispatch => Dispatch},
                stream_handlers => [lorawan_admin_logger, cowboy_compress_h, cowboy_stream_h]})
    end,
    ExtList = case application:get_env(extensions) of
        undefined ->
            undefined;
        {ok, undefined} ->
            undefined;
        {ok, List} ->
            List
    end,
    ok = start_extensions(ExtList),
    {ok, SupPid} = lorawan_sup:start_link(),
    {ok, SupPid, ExtList}.

stop(ExtList) ->
    ok = stop_extensions(ExtList),
    ok = cowboy:stop_listener(http),
    ok.

ensure_erlang_version(Min) ->
    case list_to_integer(erlang:system_info(otp_release)) of
        Num when Num >= Min -> ok;
        _Else -> {error, prerequisite_failed}
    end.

start_extensions(undefined) ->
    ok;
start_extensions([]) ->
    ok;
start_extensions([App|Rest]) ->
    {ok, _} = application:ensure_all_started(App),
    start_extensions(Rest).

stop_extensions(undefined) ->
    ok;
stop_extensions([]) ->
    ok;
stop_extensions([App|Rest]) ->
    try application:stop(App) of    % Errors in stop should not affect the system
        ok ->
            ok;
        {error, Reason} ->
            lager:error("Plugin ~w failed to stop with reason: ~w", [App, Reason])
    catch
        ExcClass:ExcType ->
            lager:error("Plugin ~w died with exception ~w:~w", [App, ExcClass, ExcType])
    end,
    stop_extensions(Rest).

% end of file
