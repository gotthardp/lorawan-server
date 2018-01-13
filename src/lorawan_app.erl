%
% Copyright (c) 2016-2018 Petr Gotthard <petr.gotthard@centrum.cz>
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
    ok = ensure_erlang_version(19),
    lorawan_db:ensure_tables(),
    Dispatch = cowboy_router:compile([
        {'_', lorawan_http_registry:static_routes()}
    ]),
    case application:get_env(lorawan_server, http_admin_listen, undefined) of
        undefined ->
            ok;
        HttpOpts ->
            {ok, _} = cowboy:start_clear(http, HttpOpts,
                #{dispatch => Dispatch,
                stream_handlers => [lorawan_admin_logger, cowboy_compress_h, cowboy_stream_h]})
    end,
    case application:get_env(lorawan_server, http_admin_listen_ssl, undefined) of
        undefined ->
            ok;
        SslOpts ->
            {ok, _} = cowboy:start_tls(https, SslOpts,
                #{dispatch => Dispatch,
                stream_handlers => [lorawan_admin_logger, cowboy_compress_h, cowboy_stream_h]})
    end,
    lorawan_sup:start_link().

stop(_State) ->
    cowboy:stop_listener(http),
    cowboy:stop_listener(https),
    ok.

ensure_erlang_version(Min) ->
    case list_to_integer(erlang:system_info(otp_release)) of
        Num when Num >= Min -> ok;
        _Else -> {error, prerequisite_failed}
    end.

% end of file
