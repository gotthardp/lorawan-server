%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_db_guard).
-behaviour(gen_server).

-export([purge_txframes/1]).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("lorawan_server_api/include/lorawan_application.hrl").
-include("lorawan.hrl").

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, _} = mnesia:subscribe({table, links, simple}),
    {ok, _} = timer:send_interval(3600*1000, trim_tables),
    {ok, undefined}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({mnesia_table_event, {delete, {Tab, Key}, _Id}}, State) ->
    handle_delete(Tab, Key),
    {noreply, State};
handle_info(trim_tables, State) ->
    trim_rxframes(),
    [mnesia:dirty_delete(events, E) || E <- expired_events()],
    {noreply, State};
handle_info(_Other, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_delete(gateways, MAC) ->
    lager:debug("Gateway ~p deleted", [lorawan_mac:binary_to_hex(MAC)]),
    % delete linked records
    ok = mnesia:dirty_delete(gateway_stats, MAC);
handle_delete(links, DevAddr) ->
    lager:debug("Node ~p deleted", [lorawan_mac:binary_to_hex(DevAddr)]),
    % delete linked records
    ok = mnesia:dirty_delete(pending, DevAddr),
    delete_matched(rxframes, #rxframe{frid='$1', devaddr=DevAddr, _='_'}),
    delete_matched(txframes, #txframe{frid='$1', devaddr=DevAddr, _='_'});
handle_delete(_Other, _Any) ->
    ok.

delete_matched(Table, Record) ->
    lists:foreach(
        fun(Id) ->
            ok = mnesia:dirty_delete(Table, Id)
        end,
        mnesia:dirty_select(Table, [{Record, [], ['$1']}])).

trim_rxframes() ->
    {ok, Count} = application:get_env(lorawan_server, retained_rxframes),
    Trimmed = lists:filter(
        fun(D) ->
            trim_rxframes(D, Count)
        end,
        lists:usort(
            mnesia:dirty_select(rxframes, [{#rxframe{devaddr='$1', _='_'}, [], ['$1']}]))),
    % log message
    if
        length(Trimmed) > 0 ->
            lager:debug("Expired rxframes from ~p",
                [[lorawan_mac:binary_to_hex(E) || E <- Trimmed]]);
        true ->
            ok
    end.

trim_rxframes(DevAddr, Count) ->
    case lorawan_db:get_last_rxframes(DevAddr, Count) of
        {[], _} ->
            false;
        {ExpRec, _} ->
            lists:foreach(fun(R) -> mnesia:dirty_delete_object(rxframes, R) end,
                ExpRec),
            true
    end.

purge_txframes(DevAddr) ->
    lists:foreach(
        fun(Obj) ->
            ok = mnesia:dirty_delete_object(txframes, Obj)
        end,
        mnesia:dirty_match_object(txframes, #txframe{devaddr=DevAddr, _='_'})).

expired_events() ->
    {ok, AgeSeconds} = application:get_env(lorawan_server, event_lifetime),
    ETime = calendar:gregorian_seconds_to_datetime(
        calendar:datetime_to_gregorian_seconds(calendar:universal_time()) - AgeSeconds),
    mnesia:dirty_select(events,
        [{#event{evid='$1', last_rx='$2', _='_'}, [{'=<', '$2', {const, ETime}}], ['$1']}]).

% end of file
