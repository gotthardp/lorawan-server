%
% Copyright (c) 2016 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_db).

-export([ensure_tables/0, trim_tables/0]).
-export([get_rxframes/1, purge_rxframes/1, purge_txframes/1]).

-include("lorawan.hrl").

ensure_tables() ->
    AllTables = [users, gateways, devices, links, txframes, rxframes],
    case has_tables(AllTables) of
        true ->
            mnesia:wait_for_tables(AllTables, 2000);
        false ->
            stopped = mnesia:stop(),
            mnesia:create_schema([node()]),
            ok = mnesia:start(),
            create_tables(),
            mnesia:wait_for_tables(AllTables, 2000),
            set_defaults()
    end.

has_tables(ReqTables) ->
    AllTables = mnesia:system_info(tables),
    lists:all(fun(Table) -> lists:member(Table, AllTables) end, ReqTables).

create_tables() ->
    mnesia:create_table(users, [
        {record_name, user},
        {attributes, record_info(fields, user)},
        {disc_copies, [node()]}]),
    mnesia:create_table(gateways, [
        {record_name, gateway},
        {attributes, record_info(fields, gateway)},
        {disc_copies, [node()]}]),
    mnesia:create_table(devices, [
        {record_name, device},
        {attributes, record_info(fields, device)},
        {disc_copies, [node()]}]),
    mnesia:create_table(links, [
        {record_name, link},
        {attributes, record_info(fields, link)},
        {disc_copies, [node()]}]),
    mnesia:create_table(txframes, [
        {type, ordered_set},
        {record_name, txframe},
        {attributes, record_info(fields, txframe)},
        {disc_copies, [node()]}]),
    mnesia:create_table(rxframes, [
        {type, ordered_set},
        {record_name, rxframe},
        {attributes, record_info(fields, rxframe)},
        {index, [mac, devaddr]},
        {disc_copies, [node()]}]).

set_defaults() ->
    lager:info("Created default user:password"),
    {ok, {User, Pass}} = application:get_env(lorawan_server, http_admin_credentials),
    mnesia:dirty_write(users, #user{name=User, pass=Pass}).

trim_tables() ->
    lists:foreach(fun(R) -> trim_rxframes(R) end,
        mnesia:dirty_all_keys(links)).

get_rxframes(DevAddr) ->
    Rec = mnesia:dirty_index_read(rxframes, DevAddr, #rxframe.devaddr),
    % split the list into expired and actual records
    if
        length(Rec) > 10 -> lists:split(length(Rec)-10, Rec);
        true -> {[], Rec}
    end.

trim_rxframes(DevAddr) ->
    {ExpRec, _} = get_rxframes(DevAddr),
    lager:debug("Expired ~w rxframes from ~w", [length(ExpRec), DevAddr]),
    lists:foreach(fun(R) -> mnesia:dirty_delete(rxframes, R) end,
        ExpRec).

purge_rxframes(DevAddr) ->
    [mnesia:dirty_delete(rxframes, Rec) ||
        Rec <- mnesia:dirty_index_read(rxframes, DevAddr, #rxframe.devaddr)].

purge_txframes(DevAddr) ->
    [mnesia:dirty_delete_object(txframes, Obj) ||
        Obj <- mnesia:dirty_match_object(txframes, #txframe{devaddr=DevAddr, _='_'})].

% end of file
