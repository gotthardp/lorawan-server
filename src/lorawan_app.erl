%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
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
    lorawan_sup:start_link().

stop(_State) ->
    ok.

ensure_erlang_version(Min) ->
    case list_to_integer(erlang:system_info(otp_release)) of
        Num when Num >= Min -> ok;
        _Else -> {error, prerequisite_failed}
    end.

% end of file
