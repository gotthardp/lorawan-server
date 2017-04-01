%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_connector_factory).
-behaviour(gen_server).

-export([start_link/0, get_or_init_connector/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("lorawan.hrl").

-define(TABLE_ID, ?MODULE).

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

get_or_init_connector(ConnId) ->
    gen_server:call({global, ?MODULE}, {get_or_init_connector, ConnId}).

init([]) ->
    process_flag(trap_exit, true),
    ets:new(?TABLE_ID, [set, named_table]),
    {ok, undefined}.

handle_call({get_or_init_connector, ConnId}, _From, State) ->
    case ets:lookup(?TABLE_ID, ConnId) of
        [{ConnId, Pid}] ->
            {reply, {ok, Pid}, State};
        [] ->
            {ok, Pid} = init_connector(ConnId),
            link(Pid),
            ets:insert(?TABLE_ID, {ConnId, Pid}),
            {reply, {ok, Pid}, State}
    end;
handle_call(_Request, _From, State) ->
    {stop, {error, unknownmsg}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, _Reason}, State) ->
    case ets:match(?TABLE_ID, {'$1', Pid}) of
        [[ConnId]] ->
            lager:debug("Connector ~s terminated", [ConnId]),
            ok = lorawan_connector_sup:delete_child(ConnId),
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


init_connector(ConnId) ->
    case mnesia:dirty_read(connectors, ConnId) of
        [Conn] ->
            lager:debug("Connecting ~s to ~s", [ConnId, Conn#connector.uri]),
            lorawan_connector_sup:start_child(Conn);
        [] ->
            {error, {not_found, ConnId}}
    end.

% end of file
