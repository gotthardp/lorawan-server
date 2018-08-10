%
% Copyright (c) 2016-2018 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_db).

-export([ensure_tables/0, ensure_table/2, ensure_table/3]).
-export([foreach_record/3, get_group/1, get_rxframes/1]).
-export([record_fields/1]).
-export([join_cluster/1, join_cluster/2, leave_cluster/1, join/1, leave/2]).

-include("lorawan.hrl").
-include("lorawan_db.hrl").

ensure_tables() ->
    case mnesia:system_info(use_dir) of
        true ->
            ok;
        false ->
            case application:get_env(lorawan_server, db_master) of
                undefined ->
                    % this is the very first node starting
                    stopped = mnesia:stop(),
                    lager:info("Database create schema"),
                    ok = mnesia:create_schema([node()]),
                    ok = mnesia:start();
                {ok, NodeName} ->
                    ok = join_cluster(NodeName)
            end
    end,
    Renamed = [
        {user, [users]},
        {server, [servers]},
        {area, [areas]},
        {gateway, [gateways]},
        {multicast_channel, [multicast_channels, multicast_groups]},
        {network, [networks]},
        {group, [groups]},
        {profile, [profiles]},
        {device, [devices]},
        {node, [nodes, links]},
        {ignored_node, [ignored_nodes, ignored_links]},
        {rxframe, [rxframes]},
        {connector, [connectors]},
        {handler, [handles]},
        {event, [events]}
    ],
    lists:foreach(fun({Name, TabDef}) -> ensure_table(Name, TabDef, Renamed) end, [
        {config, [
            {attributes, record_info(fields, config)},
            {disc_copies, [node()]}]},
        {user, [
            {attributes, record_info(fields, user)},
            {disc_copies, [node()]}]},
        {server, [
            {attributes, record_info(fields, server)},
            {disc_copies, [node()]}]},
        {area, [
            {attributes, record_info(fields, area)},
            {disc_copies, [node()]}]},
        {gateway, [
            {attributes, record_info(fields, gateway)},
            {disc_copies, [node()]}]},
        {multicast_channel, [
            {attributes, record_info(fields, multicast_channel)},
            {disc_copies, [node()]}]},
        {network, [
            {attributes, record_info(fields, network)},
            {disc_copies, [node()]}]},
        {group, [
            {attributes, record_info(fields, group)},
            {disc_copies, [node()]}]},
        {profile, [
            {attributes, record_info(fields, profile)},
            {index, [app]},
            {disc_copies, [node()]}]},
        {device, [
            {attributes, record_info(fields, device)},
            {index, [node]},
            {disc_copies, [node()]}]},
        {node, [
            {attributes, record_info(fields, node)},
            {index, [profile]},
            {disc_copies, [node()]}]},
        {ignored_node, [
            {attributes, record_info(fields, ignored_node)},
            {disc_copies, [node()]}]},
        {queued, [
            {attributes, record_info(fields, queued)},
            {index, [devaddr]},
            {disc_copies, [node()]}]},
        {pending, [
            {attributes, record_info(fields, pending)},
            {disc_copies, [node()]}]},
        {rxframe, [
            {attributes, record_info(fields, rxframe)},
            {index, [devaddr]},
            {disc_copies, [node()]}]},
        {connector, [
            {attributes, record_info(fields, connector)},
            {disc_copies, [node()]}]},
        {handler, [
            {attributes, record_info(fields, handler)},
            {disc_copies, [node()]}]},
        {event, [
            {attributes, record_info(fields, event)},
            {disc_copies, [node()]}]}
    ]).

ensure_table(Name, TabDef) ->
    ensure_table(Name, TabDef, []).

ensure_table(Name, TabDef, Renamed) ->
    case table_exists(Name) of
        true ->
            ok = mnesia:wait_for_tables([Name], 2000),
            ensure_indexes(Name, TabDef);
        false ->
            case lists:foldl(
                fun
                    (OldName, false) ->
                        case table_exists(OldName) of
                            true ->
                                rename_table(OldName, Name, TabDef),
                                true;
                            false ->
                                false
                        end;
                    (_, true) ->
                        true
                end,
                false,
                proplists:get_value(Name, Renamed, []))
            of
                true ->
                    ok;
                false ->
                    create_table(Name, TabDef)
            end

    end.

table_exists(Name) ->
    lists:member(Name, mnesia:system_info(tables)).

create_table(Name, TabDef) ->
    lager:info("Database create ~w", [Name]),
    {atomic, ok} = mnesia:create_table(Name, TabDef),
    ok = mnesia:wait_for_tables([Name], 2000),
    set_defaults(Name).

rename_table(OldName, Name, TabDef) ->
    {atomic, ok} = mnesia:create_table(Name, TabDef),
    ok = mnesia:wait_for_tables([OldName, Name], 2000),
    % copy data
    OldAttrs = mnesia:table_info(OldName, attributes),
    NewRec = proplists:get_value(record_name, TabDef, Name),
    NewAttrs = proplists:get_value(attributes, TabDef),
    lager:info("Database copy ~w ~w to ~w ~w", [OldName, OldAttrs, Name, NewAttrs]),
    lists:foreach(
        fun(Key) ->
            [Val] = mnesia:dirty_read(OldName, Key),
            % convert
            PropList = lists:zip(OldAttrs, tl(tuple_to_list(Val))),
            ok = mnesia:dirty_write(Name,
                list_to_tuple([NewRec|[get_value(Name, X, PropList) || X <- NewAttrs]]))
        end,
        mnesia:dirty_all_keys(OldName)),
    {atomic, ok} = mnesia:delete_table(OldName).

ensure_indexes(Name, TabDef) ->
    OldAttrs = mnesia:table_info(Name, attributes),
    OldIndexes = lists:sort(mnesia:table_info(Name, index)),
    NewAttrs = proplists:get_value(attributes, TabDef),
    NewIndexes =
        lists:sort(
            lists:map(fun(Key) ->
                lorawan_utils:index_of(Key, NewAttrs)+1
            end, proplists:get_value(index, TabDef, []))),
    if
        OldIndexes == NewIndexes ->
            ensure_fields(Name, TabDef);
        true ->
            lager:info("Database index update ~w: ~w to ~w", [Name, OldIndexes, NewIndexes]),
            lists:foreach(
                fun (Idx) when Idx =< length(OldAttrs)+1 ->
                        {atomic, ok} = mnesia:del_table_index(Name, lists:nth(Idx-1, OldAttrs));
                    (_) ->
                        ok
                end, lists:subtract(OldIndexes, NewIndexes)),
            ensure_fields(Name, TabDef),
            lists:foreach(
                fun (Idx) when Idx =< length(NewAttrs)+1 ->
                        {atomic, ok} = mnesia:add_table_index(Name, lists:nth(Idx-1, NewAttrs));
                    (_) ->
                        ok
                end, lists:subtract(NewIndexes, OldIndexes))
    end.

ensure_fields(Name, TabDef) ->
    OldAttrs = mnesia:table_info(Name, attributes),
    NewAttrs = proplists:get_value(attributes, TabDef),
    if
        OldAttrs == NewAttrs ->
            ok;
        true ->
            lager:info("Database fields update ~w: ~w to ~w", [Name, OldAttrs, NewAttrs]),
            {atomic, ok} = mnesia:transform_table(Name,
                fun(OldRec) ->
                    [Rec|Values] = tuple_to_list(OldRec),
                    PropList = lists:zip(OldAttrs, Values),
                    list_to_tuple([Rec|[get_value(Rec, X, PropList) || X <- NewAttrs]])
                end,
                NewAttrs),
            ok
    end.

get_value(_Rec, node, PropList) ->
    % import data from old structure
    get_value0(link, node, PropList);
get_value(server, sname, PropList) ->
    get_value0(name, sname, PropList);
get_value(user, pass_ha1, PropList) ->
    case proplists:is_defined(pass_ha1, PropList) of
        true ->
            proplists:get_value(pass_ha1, PropList);
        false ->
            lorawan_http_digest:ha1({proplists:get_value(name, PropList),
                ?REALM, proplists:get_value(pass, PropList)})
    end;
get_value(connector, publish_uplinks, PropList) ->
    get_value0(published, publish_uplinks, PropList);
get_value(connector, received, PropList) ->
    get_value0(consumed, received, PropList);
get_value(handler, app, PropList) ->
    get_value0(appid, app, PropList);
get_value(handler, uplink_fields, PropList) ->
    get_value0(fields, uplink_fields, PropList);
get_value(handler, parse_uplink, PropList) ->
    get_value0(parse, parse_uplink, PropList);
%temporary until RELEASE 0.6
get_value(site, area, PropList) ->
    get_value0(group, area, PropList);
get_value(yard, area, PropList) ->
    get_value0(group, area, PropList);
get_value(_Rec, X, PropList) ->
    proplists:get_value(X, PropList).

get_value0(Old, New, PropList) ->
    proplists:get_value(New, PropList,
      proplists:get_value(Old, PropList)).

record_fields({rxq, Freq, DatR, CodR, Time, TmSt, Rssi, LSnr}) ->
    % backward compatibility 29.4.2018
    [Freq, DatR, CodR, Time, undefined, TmSt, Rssi, LSnr];
record_fields(Record) ->
    tl(tuple_to_list(Record)).

set_defaults(config) ->
    mnesia:dirty_write(#config{name= <<"main">>, items_per_page=30});
set_defaults(user) ->
    lager:info("Database create default user:password"),
    {ok, {User, Pass}} = application:get_env(lorawan_server, http_admin_credentials),
    mnesia:dirty_write(#user{
        name=User,
        pass_ha1=lorawan_http_digest:ha1({User, ?REALM, Pass})});
set_defaults(server) ->
    mnesia:dirty_write(#server{sname=node(), router_perf=[]});
set_defaults(_Else) ->
    ok.

foreach_record(Database, Keys, Fun) ->
    lists:foreach(
        fun(Key) ->
            {atomic, ok} = mnesia:transaction(
                fun() ->
                    [Rec] = mnesia:read(Database, Key, write),
                    Rec2 = Fun(Rec),
                    if
                        Rec2 /= Rec ->
                            mnesia:write(Rec2);
                        true ->
                            ok
                    end
                end)
        end, Keys).

get_group(#node{profile=ProfName}) ->
    case mnesia:dirty_read(profile, ProfName) of
        [#profile{group=GrName}] ->
            case mnesia:dirty_read(group, GrName) of
                [Group] ->
                    Group;
                [] ->
                    undefined
            end;
        [] ->
            undefined
    end.

% returns two sorted lists {uplink frames, downlink frames}
get_rxframes(DevAddr) ->
    lists:partition(
        fun(#rxframe{dir=Dir}) ->
            if
                Dir == undefined; Dir == <<"up">>; Dir == <<"re-up">> -> true;
                Dir == <<"down">>; Dir == <<"bcast">> -> false
            end
        end,
        lists:sort(
            fun(#rxframe{frid = A}, #rxframe{frid = B}) -> A =< B end,
            mnesia:dirty_index_read(rxframe, DevAddr, #rxframe.devaddr))).

join_cluster(MasterNode, SlaveNode) ->
    Self = node(),
    case {MasterNode, SlaveNode} of
        {MasterNode, Self} ->
            join_cluster(MasterNode);
        {Self, SlaveNode} ->
            lager:info("Calling node ~s to join the cluster", [SlaveNode]),
            rpc:call(SlaveNode, lorawan_db, join, [node()], 10000)
    end.

join_cluster(NodeName) ->
    lager:info("Joining cluster ~s", [NodeName]),

    case {node(), net_adm:ping(NodeName)} of
        {NodeName, _} ->
            lager:error("Cluster: can't join myself: ~s", [NodeName]),
            {error, {cant_join_self, NodeName}};
        {_, pong} ->
            rpc:call(node(), ?MODULE, join, [NodeName], 10000);
        _ ->
            lager:error("Cluster node is unreachable: ~s", [NodeName]),
            {error, {node_unreachable, NodeName}}
    end.

leave_cluster(NodeName) ->
    lager:info("Node ~s leaving the cluster", [NodeName]),
    case {node(), net_adm:ping(NodeName)} of
        {NodeName, _} ->
            Cluster = mnesia:system_info(running_db_nodes)--[NodeName],
            rpc:call(node(), ?MODULE, leave, [Cluster, NodeName], 10000);
        {_, pong} ->
            rpc:call(NodeName, ?MODULE, leave_cluster, [NodeName], 10000);
        {_, pang} ->
            case mnesia:del_table_copy(schema, NodeName) of
                {atomic, ok} -> ok;
                {aborted, Reason} -> {error, Reason}
            end
    end.

join(NodeName) ->
    application:stop(lorawan_server),
    application:stop(mnesia),
    mnesia:delete_schema([node()]),
    application:start(mnesia),
    case mnesia:change_config(extra_db_nodes, [NodeName]) of
        {ok, [NodeName]} ->
            mnesia:change_table_copy_type(schema, node(), disc_copies),
            [ {atomic, ok} = mnesia:add_table_copy(T, node(), disc_copies)
                || T <- mnesia:system_info(tables)--[schema]],
            ok = mnesia:wait_for_tables(mnesia:system_info(local_tables), 10000),
            application:start(lorawan_server);
        {error, Reason} ->
            lager:error("Cluster copy schema: ~p", [Reason]),
            {error, Reason}
    end.

leave([], NodeName) ->
    lager:error("Node ~s is not in cluster", [NodeName]),
    {error, {no_cluster, NodeName}};
leave([Master|_], NodeName) ->
    application:stop(lorawan_server),
    application:stop(mnesia),
    rpc:call(Master, mnesia, del_table_copy, [schema, NodeName]),
    mnesia:delete_schema([node()]),
    init:stop(),
    ok.

% end of file
