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
    case {application:get_env(lorawan_server, http_admin_listen, []),
            application:get_env(lorawan_server, http_admin_listen_ssl, [])} of
        {[], []} ->
            lager:warning("Web-admin does not listen on any port"),
            ok;
        {HttpOpts, []} ->
            start_http(HttpOpts, normal_dispatch());
        {[], SslOpts} ->
            start_https(SslOpts, normal_dispatch());
        {HttpOpts, SslOpts} ->
            start_https(SslOpts, normal_dispatch()),
            start_http(HttpOpts,
                case application:get_env(lorawan_server, http_admin_redirect_ssl, true) of
                    false ->
                        normal_dispatch();
                    true ->
                        redirect_dispatch()
                end)
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

normal_dispatch() ->
    cowboy_router:compile([
        {'_', lorawan_http_registry:static_routes()}
    ]).

redirect_dispatch() ->
    Port = ranch:get_port(https),
    lager:info("Redirecting to HTTPS port ~B", [Port]),
    cowboy_router:compile([{'_',
        [{'_', lorawan_admin_redirect, #{scheme => <<"https">>, port => Port}}]}]).

start_http(Opts, Dispatch) ->
    {ok, _} = cowboy:start_clear(http, Opts, #{
        env => #{dispatch => Dispatch},
        stream_handlers => [lorawan_admin_logger, cowboy_compress_h, cowboy_stream_h]}).

start_https(Opts, Dispatch) ->
    {ok, _} = cowboy:start_tls(https, Opts, #{
        env => #{dispatch => Dispatch},
        stream_handlers => [lorawan_admin_logger, cowboy_compress_h, cowboy_stream_h]}).

% end of file
