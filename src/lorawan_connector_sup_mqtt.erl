%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_connector_sup_mqtt).
-behaviour(supervisor).

-export([start_link/0, start_child/2]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(ConnUri, Conn) ->
    supervisor:start_child(?MODULE, [ConnUri, Conn]).

init([]) ->
    {ok, {{simple_one_for_one, 10, 10}, [
        {connector,
            {lorawan_connector_mqtt, start_link, []},
            transient, 5000, worker, [lorawan_connector_mqtt]}
    ]}}.

% end of file
