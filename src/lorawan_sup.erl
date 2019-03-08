%
% Copyright (c) 2016-2019 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link(?MODULE, []).

init([]) ->
    lorawan_utils:throw_info({server, node()}, started, unique),
    {ok, {{one_for_one, 2, 10}, [
        {db_guard,
            {lorawan_db_guard, start_link, []},
            permanent, 5000, worker, [lorawan_db_guard]},
        {gateways,
            {lorawan_gw_sup, start_link, []},
            permanent, infinity, supervisor, [lorawan_gw_sup]},
        {http_registry,
            {lorawan_http_registry, start_link, []},
            permanent, 5000, worker, [lorawan_http_registry]},
        {backends,
            {lorawan_backend_sup, start_link, []},
            permanent, infinity, supervisor, [lorawan_backend_sup]}
    ]}}.

% end of file
