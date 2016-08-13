%
% Copyright (c) 2016 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-include("lorawan.hrl").

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, Port} = application:get_env(forwarder_port),
    Procs = [{packet_forwarder,
                {lorawan_iface_forwarder, start_link, [Port]},
                permanent, 5000, worker, [lorawan_iface_forwarder]}],
    {ok, {{one_for_one, 10, 10}, Procs}}.

% end of file
