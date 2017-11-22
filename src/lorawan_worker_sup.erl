%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_worker_sup).
-behaviour(supervisor).

-export([start_link/0, start_child/2]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child() ->
    supervisor:start_child(?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 10, 10}, [
        {worker,
            {lorawan_worker, start_link, []},
            transient, 5000, worker, [lorawan_worker]}
    ]}}.

% end of file
