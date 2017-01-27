%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_db).

-export([ensure_tables/0, ensure_table/2, trim_tables/0]).
-export([get_rxframes/1, purge_rxframes/1, purge_txframes/1]).

-include_lib("lorawan_server_api/include/lorawan_application.hrl").
-include("lorawan.hrl").

ensure_tables() ->
    case mnesia:system_info(use_dir) of
        true ->
            ok;
        false ->
            stopped = mnesia:stop(),
            lager:info("Database create schema"),
            mnesia:create_schema([node()]),
            ok = mnesia:start()
    end,
    lists:foreach(fun({Name, TabDef}) -> ensure_table(Name, TabDef) end, [
        {users, [
            {record_name, user},
            {attributes, record_info(fields, user)},
            {disc_copies, [node()]}]},
        {gateways, [
            {record_name, gateway},
            {attributes, record_info(fields, gateway)},
            {disc_copies, [node()]}]},
        {devices, [
            {record_name, device},
            {attributes, record_info(fields, device)},
            {disc_copies, [node()]}]},
        {links, [
            {record_name, link},
            {attributes, record_info(fields, link)},
            {disc_copies, [node()]}]},
        {ignored_links, [
            {record_name, ignored_link},
            {attributes, record_info(fields, ignored_link)},
            {disc_copies, [node()]}]},
        {pending, [
            {record_name, pending},
            {attributes, record_info(fields, pending)},
            {disc_copies, [node()]}]},
        {txframes, [
            {type, ordered_set},
            {record_name, txframe},
            {attributes, record_info(fields, txframe)},
            {disc_copies, [node()]}]},
        {rxframes, [
            {record_name, rxframe},
            {attributes, record_info(fields, rxframe)},
            {index, [mac, devaddr]},
            {disc_copies, [node()]}]}
    ]).

ensure_table(Name, TabDef) ->
    case lists:member(Name, mnesia:system_info(tables)) of
        true ->
            mnesia:wait_for_tables([Name], 2000),
            ensure_fields(Name, TabDef);
        false ->
            lager:info("Database create ~w", [Name]),
            mnesia:create_table(Name, TabDef),
            mnesia:wait_for_tables([Name], 2000),
            set_defaults(Name)
    end.

ensure_fields(Name, TabDef) ->
    OldAttrs = mnesia:table_info(Name, attributes),
    NewAttrs = proplists:get_value(attributes, TabDef),
    if
        OldAttrs == NewAttrs ->
            ok;
        true ->
            lager:info("Database update ~w: ~w to ~w", [Name, OldAttrs, NewAttrs]),
            {atomic, ok} = mnesia:transform_table(Name,
                fun(OldRec) ->
                    [Rec|Values] = tuple_to_list(OldRec),
                    PropList = lists:zip(OldAttrs, Values),
                    list_to_tuple([Rec|[proplists:get_value(X, PropList) || X <- NewAttrs]])
                end,
                NewAttrs)
    end.

set_defaults(users) ->
    lager:info("Database create default user:password"),
    {ok, {User, Pass}} = application:get_env(lorawan_server, http_admin_credentials),
    mnesia:dirty_write(users, #user{name=User, pass=Pass});
set_defaults(_Else) ->
    ok.

trim_tables() ->
    lists:foreach(fun(R) -> trim_rxframes(R) end,
        mnesia:dirty_all_keys(links)).

get_rxframes(DevAddr) ->
    Rec = mnesia:dirty_index_read(rxframes, DevAddr, #rxframe.devaddr),
    SRec = lists:sort(fun(#rxframe{frid = A}, #rxframe{frid = B}) -> A < B end, Rec),
    % split the list into expired and actual records
    if
        length(SRec) > 50 -> lists:split(length(SRec)-50, SRec);
        true -> {[], SRec}
    end.

trim_rxframes(DevAddr) ->
    case get_rxframes(DevAddr) of
        {[], _} ->
            ok;
        {ExpRec, _} ->
            lager:debug("Expired ~w rxframes from ~w", [length(ExpRec), DevAddr]),
            lists:foreach(fun(R) -> mnesia:dirty_delete_object(rxframes, R) end,
                ExpRec)
    end.

purge_rxframes(DevAddr) ->
    [mnesia:dirty_delete_object(rxframes, Rec) ||
        Rec <- mnesia:dirty_index_read(rxframes, DevAddr, #rxframe.devaddr)].

purge_txframes(DevAddr) ->
    [mnesia:dirty_delete_object(txframes, Obj) ||
        Obj <- mnesia:dirty_match_object(txframes, #txframe{devaddr=DevAddr, _='_'})].

% end of file
