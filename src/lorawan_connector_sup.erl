%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_connector_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link(?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 10, 10}, [
        {mqtt,
            {lorawan_connector_sup_mqtt, start_link, []},
            permanent, infinity, supervisor, [lorawan_connector_sup_mqtt]},
        {http,
            {lorawan_connector_sup_http, start_link, []},
            permanent, infinity, supervisor, [lorawan_connector_sup_http]},
        {factory,
            {lorawan_connector_factory, start_link, []},
            permanent, 5000, worker, [lorawan_connector_factory]}
    ]}}.

% end of file
