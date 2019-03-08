%
% Copyright (c) 2016-2019 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_handler_sup).
-behaviour(supervisor).

-export([start_link/0, start_child/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child() ->
    supervisor:start_child(?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 2, 10}, [
        {handler,
            {lorawan_handler, start_link, []},
            transient, 5000, worker, [lorawan_handler]}
    ]}}.

% end of file
