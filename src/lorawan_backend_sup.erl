%
% Copyright (c) 2016-2019 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_backend_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link(?MODULE, []).

init([]) ->
    {ok, {{one_for_all, 2, 10}, [
        {connectors,
            {lorawan_connector_sup, start_link, []},
            permanent, infinity, supervisor, [lorawan_connector_sup]},
        {factory,
            {lorawan_backend_factory, start_link, []},
            permanent, 5000, worker, [lorawan_backend_factory]}
    ]}}.

% end of file
