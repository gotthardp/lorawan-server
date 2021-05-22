%
% Copyright (c) 2016-2019 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_connector_monitor).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("lorawan_db.hrl").

-record(state, {period}).

% Default connector health check period 10 seconds
-define(DEFAULT_PERIOD, 10000).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Period = application:get_env(lorawan_server, connector_monitor_period,
            ?DEFAULT_PERIOD),
    {ok, #state{period=Period}, Period}.

handle_call(_Request, _From, State) ->
    {stop, {error, unknownmsg}, State}.

handle_cast(_Msg, #state{period=Period}=State) ->
    {noreply, State, Period}.

handle_info(timeout, #state{period=Period}=State) ->
    % run through known connectors, attempt restart if failed with 'network'
    lists:foreach(
        fun(ConnId) ->
            [Connector] = mnesia:dirty_read(connector, ConnId),
            restart_connector(Connector)
        end,
        mnesia:dirty_all_keys(connector)),
    {noreply, State, Period};

handle_info(Info, #state{period=Period}=State) ->
    lager:debug("unknown info ~p", [Info]),
    {noreply, State, Period}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

restart_connector(#connector{enabled=true, failed=[<<"network">>]}=Connector) ->
    {atomic, ok} = mnesia:transaction(
        fun() ->
            lorawan_admin:write(Connector#connector{failed=[]})
        end);
restart_connector(_Connector) ->
    ok.
