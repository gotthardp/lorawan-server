%
% Copyright (c) 2016-2019 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_connector_sup).
-behaviour(supervisor).

-export([start_link/0, start_child/3, stop_child/1]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Id, Module, Args) ->
    lager:debug("Start ~p", [Id]),
    supervisor:start_child(?MODULE,
        {Id,
            {Module, start_link, Args},
            transient, 5000, worker, [Module]}).

stop_child(Id) ->
    lager:debug("Stop ~p", [Id]),
    _ = supervisor:terminate_child(?MODULE, Id),
    _ = supervisor:delete_child(?MODULE, Id).

init([]) ->
    % dynamically managed children, one for each connector
    {ok, {{one_for_one, 2, 10}, []}}.

% end of file
