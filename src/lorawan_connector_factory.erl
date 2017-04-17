%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_connector_factory).
-behaviour(gen_server).

-export([start_link/0, get_or_init_connector/1, publish/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("lorawan.hrl").

-define(TABLE_ID, ?MODULE).

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

get_or_init_connector(ConnId) ->
    gen_server:call({global, ?MODULE}, {get_or_init_connector, ConnId}).

publish(ConnId, Msg, Vars) ->
    case get_or_init_connector(ConnId) of
        {ok, Pid} ->
            gen_server:cast(Pid, {publish, Msg, Vars});
        {error, Error} ->
            lager:error("Error ~w", [Error])
    end.

init([]) ->
    process_flag(trap_exit, true),
    ets:new(?TABLE_ID, [set, named_table]),
    % start connectors that need to subscribe
    lists:foreach(
        fun(Conn) ->
            init_connector(Conn)
        end,
        mnesia:dirty_select(connectors,
            [{#connector{enabled='$1', subscribe='$2', _='_'},
             [{'==', '$1', true}, {'=/=', '$2', undefined}, {'=/=', '$2', <<>>}], ['$_']}])),
    {ok, undefined}.

handle_call({get_or_init_connector, ConnId}, _From, State) ->
    case ets:lookup(?TABLE_ID, ConnId) of
        [{ConnId, Pid}] ->
            {reply, {ok, Pid}, State};
        [] ->
            {reply, init_connector_id(ConnId), State}
    end;
handle_call(_Request, _From, State) ->
    {stop, {error, unknownmsg}, State}.

init_connector_id(ConnId) ->
    case mnesia:dirty_read(connectors, ConnId) of
        [#connector{enabled=false}] ->
            {error, {disabled, ConnId}};
        [Conn] ->
            init_connector(Conn);
        [] ->
            {error, {not_found, ConnId}}
    end.

init_connector(Conn) ->
    lager:info("Connecting ~s to ~s", [Conn#connector.connid, Conn#connector.uri]),
    case lorawan_connector_sup:start_child(Conn) of
        {ok, Pid} ->
            link(Pid),
            ets:insert(?TABLE_ID, {Conn#connector.connid, Pid}),
            {ok, Pid};
        Error ->
            Error
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, _Reason}, State) ->
    case ets:match(?TABLE_ID, {'$1', Pid}) of
        [[ConnId]] ->
            lager:debug("Connector ~s terminated", [ConnId]),
            lorawan_connector_sup:delete_child(ConnId),
            ets:delete(?TABLE_ID, ConnId),
            {noreply, State};
        [] ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% end of file
