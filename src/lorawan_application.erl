%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_application).

-export([init/0, handle_join/3, handle_rx/4, encode_tx/4]).
-export([store_frame/2, send_stored_frames/2, downlink/3, multicast/3]).

-define(MAX_DELAY, 250). % milliseconds

-include("lorawan_application.hrl").
-include("lorawan.hrl").

-callback init(App :: binary()) ->
    ok | {ok, [Path :: cowboy_router:route_path()]}.
-callback handle_join(Gateway :: #gateway{}, Device :: #device{}, Link :: #node{}) ->
    ok | {error, Error :: term()}.
-callback handle_rx(Gateway :: #gateway{}, Link :: #node{}, RxData :: #rxdata{}, RxQ :: #rxq{}) ->
    ok | retransmit |
    {send, Port :: integer(), Data :: #txdata{}} |
    {error, Error :: term()}.

init() ->
    {ok, Modules} = application:get_env(lorawan_server, plugins),
    do_init(Modules, []).

do_init([], Acc) -> {ok, Acc};
do_init([Application|Rest], Acc) ->
    case invoke_init(Application) of
        ok -> do_init(Rest, Acc);
        {ok, Handlers} -> do_init(Rest, Acc++Handlers);
        Else -> Else
    end.

invoke_init({App, {AppName, Module}}) ->
    {ok, _Started} = application:ensure_all_started(AppName),
    apply(Module, init, [App]);
invoke_init({App, Module}) when is_atom(Module) ->
    apply(Module, init, [App]).

store_frame(DevAddr, TxData) ->
    {atomic, ok} =
        mnesia:transaction(fun() ->
            mnesia:write(txframes, #txframe{frid= <<(erlang:system_time()):64>>,
                datetime=calendar:universal_time(), devaddr=DevAddr, txdata=TxData}, write)
        end),
    ok.

send_stored_frames(DevAddr, DefPort) ->
    case mnesia:dirty_select(txframes, [{#txframe{devaddr=DevAddr, _='_'}, [], ['$_']}]) of
        [] ->
            mnesia:subscribe({table, txframes, simple}),
            receive
                % the record name returned is 'txframes' regardless any record_name settings
                {mnesia_table_event, {write, TxFrame, _ActivityId}} when element(#txframe.devaddr, TxFrame) == DevAddr ->
                    transmit_and_delete(DefPort, setelement(1, TxFrame, txframe), false)
                after ?MAX_DELAY ->
                    ok
            end;
        [TheOnly] ->
            transmit_and_delete(DefPort, TheOnly, false);
        [First|_Tail] ->
            transmit_and_delete(DefPort, First, true)
    end.

transmit_and_delete(DefPort, TxFrame, Pending) ->
    ok = mnesia:dirty_delete(txframes, TxFrame#txframe.frid),
    TxData = TxFrame#txframe.txdata,
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
