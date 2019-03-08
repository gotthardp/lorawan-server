%
% Copyright (c) 2016-2019 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_application).

-export([init/0]).
-export([store_frame/2, get_stored_frames/1, take_previous_frames/2, send_stored_frames/2]).

-define(MAX_DELAY, 250). % milliseconds

-include("lorawan.hrl").
-include("lorawan_db.hrl").

-callback init(App :: binary()) ->
    ok | {ok, [Path :: cowboy_router:route_path()]}.
-callback handle_join({Network :: #network{}, Profile :: #profile{}, Device :: #device{}},
        {MAC :: binary(), RxQ :: #rxq{}}, DevAddr :: devaddr()) ->
    ok | {error, Error :: term()}.
-callback handle_uplink({Network :: #network{}, Profile :: #profile{}, Node :: #node{}},
        {MAC :: binary(), RxQ :: #rxq{}}, {lost, Receipt :: any()}, Frame :: #frame{}) ->
    retransmit | {ok, State :: any()} | {error, Error :: term()}.
-callback handle_rxq({Network :: #network{}, Profile :: #profile{}, Node :: #node{}},
        Gateways :: [{MAC :: binary(), RxQ :: #rxq{}}], WillReply :: boolean(), Frame :: #frame{}, State :: any()) ->
    ok | {send, Data :: #txdata{}} | {error, Error :: term()}.
-callback handle_delivery({Network :: #network{}, Profile :: #profile{}, Node :: #node{}},
        Result :: atom(), Receipt :: any()) ->
    ok.

init() ->
    Modules = application:get_env(lorawan_server, applications, []),
    do_init(Modules, []).

do_init([], Acc) ->
    {ok, Acc};
do_init([AppModule | Rest], Acc) ->
    case invoke_init(AppModule) of
        ok ->
            do_init(Rest, Acc);
        {ok, Handlers} ->
            do_init(Rest, Acc++Handlers);
        Else ->
            Else
    end.

invoke_init({App, {AppName, Module}}) ->
    {ok, _Started} = application:ensure_all_started(AppName),
    apply(Module, init, [App]);
invoke_init({App, Module}) when is_atom(Module) ->
    apply(Module, init, [App]).

store_frame(DevAddr, TxData) ->
    {ok, FrId} = eid:get_bin(),
    mnesia:dirty_write(#queued{frid=FrId,
        datetime=calendar:universal_time(), devaddr=DevAddr, txdata=TxData}).

get_stored_frames(DevAddr) ->
    mnesia:dirty_select(queued, [{#queued{devaddr=DevAddr, _='_'}, [], ['$_']}]).

take_previous_frames(DevAddr, Port) ->
    lists:foldl(
        fun(#queued{frid=Id, txdata=TxData}, Acc) ->
            if
                TxData#txdata.port == Port ->
                    ok = mnesia:dirty_delete(queued, Id),
                    [TxData | Acc];
                true ->
                    Acc
            end
        end,
        [], get_stored_frames(DevAddr)).

send_stored_frames(DevAddr, DefPort) ->
    case get_stored_frames(DevAddr) of
        [] ->
            mnesia:subscribe({table, queued, simple}),
            receive
                {mnesia_table_event, {write, #queued{devaddr=DevAddr}=TxFrame, _ActivityId}} ->
                    transmit_and_delete(DefPort, TxFrame, false)
                after ?MAX_DELAY ->
                    ok
            end;
        [TheOnly] ->
            transmit_and_delete(DefPort, TheOnly, false);
        [First|_Tail] ->
            transmit_and_delete(DefPort, First, true)
    end.

transmit_and_delete(DefPort, TxFrame, Pending) ->
    ok = mnesia:dirty_delete(queued, TxFrame#queued.frid),
    TxData = TxFrame#queued.txdata,
    % raw websocket does not define port
    OutPort = if
        TxData#txdata.port == undefined -> DefPort;
        true -> TxData#txdata.port
    end,
    % add the pending flag
    OutPending = if
        TxData#txdata.pending == undefined -> Pending;
        true -> TxData#txdata.pending
    end,
    {send, TxData#txdata{port=OutPort, pending=OutPending}}.

% end of file
