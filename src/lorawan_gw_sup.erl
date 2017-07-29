%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_gw_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link(?MODULE, []).

init([]) ->
    {ok, PktFwdOpts} = application:get_env(packet_forwarder_listen),
    {ok, {{one_for_one, 10, 10}, [
        {gateway_router,
            {lorawan_gw_router, start_link, []},
            permanent, 5000, worker, [lorawan_gw_router]},
        {packet_forwarder,
            {lorawan_gw_forwarder, start_link, [PktFwdOpts]},
            permanent, 5000, worker, [lorawan_gw_forwarder]},
        {handler_pool,
            {wpool, start_pool, [handler_pool, [
                {workers, 20},
                {overrun_warning, 500},
                {worker, {lorawan_worker, []}}
            ]]},
            permanent, infinity, supervisor, []}
    ]}}.

% end of file
