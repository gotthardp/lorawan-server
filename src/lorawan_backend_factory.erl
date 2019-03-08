%
% Copyright (c) 2016-2019 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_backend_factory).
-behaviour(gen_server).

-export([start_link/0, uplink/3, event/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([nodes_with_backend/1]).

-include("lorawan_db.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    % track database updates to keep the processes in-sync
    {ok, _} = mnesia:subscribe({table, node, detailed}),
    {ok, _} = mnesia:subscribe({table, profile, detailed}),
    {ok, _} = mnesia:subscribe({table, connector, detailed}),
    % start connectors that need to subscribe
    self() ! start_all,
    {ok, undefined}.

handle_call(_Request, _From, State) ->
    {stop, {error, unknownmsg}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(start_all, State) ->
    lists:foreach(
        fun(ConnId) ->
            [Connector] = mnesia:dirty_read(connector, ConnId),
            start_connector(Connector)
        end,
        mnesia:dirty_all_keys(connector)),
    {noreply, State};
% ignore schema changes
handle_info({mnesia_table_event, {write, schema, _NewRec, _OldRec, _Activity}}, State) ->
    {noreply, State};
handle_info({mnesia_table_event, {delete, schema, _What, _OldRec0, _Activity}}, State) ->
    {noreply, State};
% handle database updates
handle_info({mnesia_table_event, {write, _Table, NewRec0, [], _Activity}}, State) ->
    item_created(NewRec0),
    {noreply, State};
handle_info({mnesia_table_event, {write, _Table, NewRec0, [OldRec0], _Activity}}, State) ->
    item_updated(NewRec0, OldRec0),
    {noreply, State};
handle_info({mnesia_table_event, {delete, _Table, _What, [OldRec0], _Activity}}, State) ->
    item_deleted(OldRec0),
    {noreply, State};

handle_info(Info, State) ->
    lager:debug("unknown info ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

item_created(#node{profile=ProfId}) ->
    announce_profile_update(ProfId);
item_created(#profile{app=App}) ->
    announce_backend_update(App);
item_created(#connector{}=Connector) ->
    start_connector(Connector).

item_updated(#node{devaddr=DevAddr, profile=Profile, appargs=Args},
        #node{devaddr=DevAddr, profile=Profile, appargs=Args}) ->
    % nothing significant has changed
    ok;
item_updated(#node{devaddr=DevAddr, profile=ProfId1}, #node{devaddr=DevAddr, profile=ProfId2}) ->
    % the node was moved from one profile to another, so two set of connectors may be affected
    announce_profile_update(ProfId2),
    announce_profile_update(ProfId1);
item_updated(#profile{name=Name, app=App}, #profile{name=Name, app=App}) ->
    % nothing significant has changed
    ok;
item_updated(#profile{name=Name, app=App1}, #profile{name=Name, app=App2}) ->
    announce_backend_update(App2),
    announce_backend_update(App1);
item_updated(#connector{connid=ConnId}=Connector1, #connector{connid=ConnId}=Connector2) ->
    stop_connector(Connector2),
    start_connector(Connector1).

item_deleted(#node{profile=ProfId}) ->
    announce_profile_update(ProfId);
item_deleted(#profile{app=App}) ->
    announce_backend_update(App);
item_deleted(#connector{}=Connector) ->
    stop_connector(Connector).


start_connector(#connector{connid=Id, app=App, uri=Uri, enabled=true,
        failed=Failed}=Connector) when Failed == undefined; Failed == [] ->
    case find_module(Uri) of
        {ok, Module} ->
            pg2:create({backend, App}),
            apply(Module, start_connector, [Connector]);
        {error, Error} ->
            lorawan_utils:throw_error({connector, Id}, Error)
    end;
start_connector(#connector{}) ->
    ok.

stop_connector(#connector{connid=Id, uri=Uri, enabled=true, failed=Failed})
        when Failed == undefined; Failed == [] ->
    case find_module(Uri) of
        {ok, Module} ->
            apply(Module, stop_connector, [Id]);
        {error, Error} ->
            lorawan_utils:throw_error({connector, Id}, Error)
    end;
stop_connector(#connector{}) ->
    ok.

find_module(Uri) ->
    case binary:split(Uri, [<<":">>]) of
        [Scheme | _] ->
            Known = application:get_env(lorawan_server, connectors, []),
            find_module0(Scheme, Known);
        _Else ->
            {error, invalid_uri}
    end.

find_module0(Scheme, [{Module, SchemeList} | Other]) ->
    case lists:member(Scheme, SchemeList) of
        true ->
            {ok, Module};
        false ->
            find_module0(Scheme, Other)
    end;
find_module0(Scheme, []) ->
    {error, {unknown_scheme, Scheme}}.


-spec uplink(binary(), {#profile{}, #node{}} | {#profile{}, #device{}, binary()}, map()) -> ok.
uplink(App, Node, Vars) ->
    send_to_connectors(App, {uplink, Node, Vars}).

-spec event(binary(), {#profile{}, #node{}} | {#profile{}, #device{}, binary()}, map()) -> ok.
event(App, Node, Vars) ->
    send_to_connectors(App, {event, Node, Vars}).

announce_profile_update(ProfId) ->
    case mnesia:dirty_read(profile, ProfId) of
        [#profile{app=App}] ->
            announce_backend_update(App);
        _Else ->
            []
    end.

announce_backend_update(App) ->
    send_to_connectors(App, nodes_changed).

nodes_with_backend(App) ->
    lists:foldl(
        fun(#profile{name=ProfId}=Profile, Acc) ->
            lists:foldl(
                fun(Node, Acc2) ->
                    [{Profile, Node} | Acc2]
                end,
                Acc, mnesia:dirty_index_read(node, ProfId, #node.profile))
        end,
        [], mnesia:dirty_index_read(profile, App, #profile.app)).

send_to_connectors(App, Message) ->
    case pg2:get_members({backend, App}) of
        {error, _Error} ->
            % the application is internal or not defined
            ok;
        [] ->
            lager:warning("Message not sent to any connector");
        List when is_list(List) ->
            lists:foreach(
                fun(Pid) -> Pid ! Message end,
                List)
    end.

% end of file
