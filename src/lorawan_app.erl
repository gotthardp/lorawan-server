%
% Copyright (c) 2016-2019 Petr Gotthard <petr.gotthard@centrum.cz>
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
    ok = ensure_erlang_version(21),
    lager:debug("Using config: ~p", [application:get_all_env(lorawan_server)]),
    lorawan_db:ensure_tables(),
    case {application:get_env(lorawan_server, http_admin_listen, []), retrieve_valid_ssl()} of
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

retrieve_valid_ssl() ->
    case application:get_env(lorawan_server, http_admin_listen_ssl, []) of
        [] ->
            [];
        Config ->
            case file_configured(certfile, Config) and file_configured(keyfile, Config) of
                false ->
                    lager:warning("http_admin_listen_ssl not configured"),
                    [];
                true ->
                    Config
            end
    end.

file_configured(Name, Config) ->
    case proplists:get_value(Name, Config) of
        undefined ->
            false;
        File ->
            case file:read_file_info(File) of
                {ok, _} ->
                    true;
                {error, _} ->
                    false
            end
    end.

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
        {'_', lorawan_http_registry:get_static(routes)++lorawan_http_registry:get_custom(routes)}
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
