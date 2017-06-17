%
% Copyright (c) 2016 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    lorawan_utils:throw_info(server, started),
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
                {overrun_warning, 200},
                {worker, {lorawan_worker, []}}
            ]]},
            permanent, 5000, supervisor, []},
        {connector_sup,
            {lorawan_connector_sup, start_link, []},
            permanent, 5000, supervisor, [lorawan_connector_sup]},
        {connector_factory,
            {lorawan_connector_factory, start_link, []},
            permanent, 5000, worker, [lorawan_connector_factory]}
    ]}}.

% end of file
