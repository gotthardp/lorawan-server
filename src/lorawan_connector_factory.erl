%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_connector_factory).
-behaviour(gen_server).

-export([start_link/0, publish/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("lorawan.hrl").

-define(TABLE_ID, ?MODULE).

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

publish(ConnId, Msg, Vars) ->
    case ensure_connected(ConnId) of
        {ok, Pid} ->
            gen_server:cast(Pid, {publish, Msg, Vars});
        {error, Error} ->
            {error, Error}
    end.

ensure_connected(ConnId) ->
    gen_server:call({global, ?MODULE}, {ensure_connected, ConnId}).

init([]) ->
    process_flag(trap_exit, true),
    _Table = ets:new(?TABLE_ID, [set, named_table]),
    % start connectors that need to subscribe
    lists:foreach(
        fun(Conn) ->
            init_connector(Conn)
        end,
        mnesia:dirty_select(connectors,
            [{#connector{enabled='$1', subscribe='$2', _='_'},
             [{'==', '$1', true}, {'=/=', '$2', undefined}, {'=/=', '$2', <<>>}], ['$_']}])),
    {ok, _} = mnesia:subscribe({table, connectors, simple}),
    {ok, undefined}.

handle_call({ensure_connected, ConnId}, _From, State) ->
    {reply, get_or_init_connector_id(ConnId), State};
handle_call({ensure_disconnected, ConnId}, _From, State) ->
    {reply, maybe_terminate_connector_id(ConnId), State};
handle_call(_Request, _From, State) ->
    {stop, {error, unknownmsg}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, Reason}, State) ->
    case ets:match(?TABLE_ID, {'$1', Pid}) of
        [[ConnId]] ->
            handle_exit(ConnId, Reason),
            {noreply, State};
        [] ->
            {noreply, State}
    end;

handle_info({mnesia_table_event, {write, Record0, _Trans}}, State) ->
    case setelement(1, Record0, connector) of
        COn when COn#connector.enabled==true, COn#connector.subscribe =/= undefined ->
            get_or_init_subscription(COn);
        COff ->
            maybe_terminate_connector_id(COff#connector.connid)
    end,
    {noreply, State};
handle_info({mnesia_table_event, {delete, {connectors, ConnId}, _Trans}}, State) ->
    maybe_terminate_connector_id(ConnId),
    {noreply, State};

handle_info(Info, State) ->
    lager:debug("unknown info ~w", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


get_or_init_connector_id(ConnId) ->
    case ets:lookup(?TABLE_ID, ConnId) of
        [{ConnId, Pid}] ->
            {ok, Pid};
        [] ->
            init_connector_id(ConnId)
    end.

init_connector_id(ConnId) ->
    case mnesia:dirty_read(connectors, ConnId) of
        [#connector{enabled=false}] ->
            {error, {disabled, ConnId}};
        [Conn] ->
            init_connector(Conn);
        [] ->
            {error, {not_found, ConnId}}
    end.

get_or_init_subscription(#connector{connid=ConnId, enabled=false}) ->
    {error, {disabled, ConnId}};
get_or_init_subscription(#connector{connid=ConnId, subscribe=Topic} = Conn) ->
    case ets:lookup(?TABLE_ID, ConnId) of
        [{ConnId, Pid}] ->
            gen_server:call(Pid, {resubscribe, Topic}),
            {ok, Pid};
        [] ->
            init_connector(Conn)
    end.

init_connector(Conn) ->
    case lorawan_connector_sup:start_child(Conn) of
        {ok, Pid} ->
            init_connector0(Conn, Pid);
        {error, {already_started, Pid}} ->
            % the process got rested by its supervisor
            init_connector0(Conn, Pid);
        Error ->
            lager:error("Cannot connect ~w: ~w", [Conn#connector.connid, Error]),
            Error
    end.

init_connector0(Conn, Pid) ->
    link(Pid),
    ets:insert(?TABLE_ID, {Conn#connector.connid, Pid}),
    {ok, Pid}.

maybe_terminate_connector_id(ConnId) ->
    case ets:lookup(?TABLE_ID, ConnId) of
        [{ConnId, Pid}] ->
            gen_server:call(Pid, disconnect);
        [] ->
            ok
    end.

handle_exit(ConnId, Reason) ->
    ok = lorawan_connector_sup:delete_child(ConnId),
    ets:delete(?TABLE_ID, ConnId),
    handle_exit2(ConnId, Reason).

handle_exit2(_ConnId, normal) ->
    ok;
handle_exit2(ConnId, Error) ->
    lager:error("Connector ~s terminated: ~w", [ConnId, Error]),
    % make sure we don't start it again
    [Conn] = mnesia:dirty_read(connectors, ConnId),
    mnesia:dirty_write(connectors, Conn#connector{enabled=false}).

% end of file
