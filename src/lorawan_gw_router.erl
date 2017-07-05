%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_gw_router).
-behaviour(gen_server).

-export([start_link/0]).
-export([register/3, status/2, uplinks/1, downlink/4, downlink_error/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("lorawan_server_api/include/lorawan_application.hrl").
-include("lorawan.hrl").

-record(state, {pulladdr, recent}).

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

register(MAC, Process, Target) ->
    gen_server:cast({global, ?MODULE}, {register, MAC, Process, Target}).

status(MAC, S) ->
    gen_server:cast({global, ?MODULE}, {status, MAC, S}).

uplinks(PkList) ->
    gen_server:cast({global, ?MODULE}, {uplinks, PkList}).

downlink(MAC, DevAddr, TxQ, PHYPayload) ->
    {EIRPdef, EIRPmax} = lorawan_mac_region:eirp_limits(TxQ#txq.region),
    [Gateway] = mnesia:dirty_read(gateways, MAC),
    Power = value_or_default(Gateway#gateway.tx_powe, EIRPdef),
    Gain = value_or_default(Gateway#gateway.ant_gain, 0),
    gen_server:cast({global, ?MODULE}, {downlink, MAC, DevAddr,
        TxQ#txq{powe=erlang:min(Power, EIRPmax-Gain)}, Gateway#gateway.tx_rfch, PHYPayload}).

downlink_error(MAC, Opaque, Error) ->
    gen_server:cast({global, ?MODULE}, {downlink_error, MAC, Opaque, Error}).

value_or_default(Num, _Def) when is_number(Num) -> Num;
value_or_default(_Num, Def) -> Def.

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{pulladdr=dict:new(), recent=dict:new()}}.

handle_call(_Request, _From, State) ->
    {stop, {error, unknownmsg}, State}.

handle_cast({register, MAC, Process, {Host, Port, _}=Target}, #state{pulladdr=Dict}=State) ->
    case dict:find(MAC, Dict) of
        {ok, {Process, Target}} ->
            {noreply, State};
        _Else ->
            lorawan_utils:throw_info({gateway, MAC}, {connected, {Host, Port}}),
            Dict2 = dict:store(MAC, {Process, Target}, Dict),
            {noreply, State#state{pulladdr=Dict2}}
    end;

handle_cast({status, MAC, S}, State) ->
    case lorawan_mac:process_status(MAC, S) of
        ok ->
            ok;
        {error, {Object, Error}} ->
            lorawan_utils:throw_error(Object, Error)
    end,
    {noreply, State};

handle_cast({uplinks, PkList}, #state{recent=Recent}=State) ->
    % due to reflections the gateways may have received the same frame twice
    % reflected frames received by the same gateway are ignored
    Unique = remove_duplicates(PkList, []),
    % wait for packet receptions from other gateways
    Recent2 =
        lists:foldl(
            fun(Frame, Dict) -> store_frame(Frame, Dict) end,
            Recent, Unique),
    {noreply, State#state{recent=Recent2}};

handle_cast({downlink, MAC, DevAddr, TxQ, RFCh, PHYPayload}, #state{pulladdr=Dict}=State) ->
    % lager:debug("<-- datr ~s, codr ~s, tmst ~B, size ~B", [TxQ#txq.datr, TxQ#txq.codr, TxQ#txq.tmst, byte_size(PHYPayload)]),
    case dict:find(MAC, Dict) of
        {ok, {Process, Target}} ->
            % send data to the gateway interface handler
            gen_server:cast(Process, {send, Target, DevAddr, TxQ, RFCh, PHYPayload});
        error ->
            lager:warning("Downlink request ignored. Gateway ~w not connected.", [MAC])
    end,
    {noreply, State};

handle_cast({downlink_error, MAC, undefined, Error}, State) ->
    lorawan_utils:throw_error({gateway, MAC}, Error),
    {noreply, State};
handle_cast({downlink_error, _MAC, DevAddr, Error}, State) ->
    lorawan_utils:throw_error({node, DevAddr}, Error),
    {noreply, State}.


handle_info({process, PHYPayload}, #state{recent=Recent}=State) ->
    % find the best (for now)
    [{MAC, RxQ}|_Rest] = lists:sort(
        fun({_M1, Q1}, {_M2, Q2}) ->
            Q1#rxq.rssi >= Q2#rxq.rssi
        end,
        dict:fetch(PHYPayload, Recent)),
    % lager:debug("--> datr ~s, codr ~s, tmst ~B, size ~B", [RxQ#rxq.datr, RxQ#rxq.codr, RxQ#rxq.tmst, byte_size(PHYPayload)]),
    wpool:cast(handler_pool, {MAC, RxQ, PHYPayload}, available_worker),
    Recent2 = dict:erase(PHYPayload, Recent),
    {noreply, State#state{recent=Recent2}};
% handler termination
handle_info({'EXIT', _FromPid, normal}, State) ->
    {noreply, State};
handle_info({'EXIT', _FromPid, Reason}, State) ->
    lager:error("Hanler terminated: ~p", [Reason]),
    {noreply, State}.

terminate(Reason, _State) ->
    % record graceful shutdown in the log
    lager:info("gateway router terminated: ~p", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


remove_duplicates([{MAC, RxQ, PHYPayload} | Tail], Unique) ->
    % check if the first element is duplicate
    case lists:keytake(PHYPayload, 3, Tail) of
        {value, {MAC2, RxQ2, PHYPayload}, Tail2} ->
            % select element of a better quality and re-check for other duplicates
            if
                RxQ#rxq.rssi >= RxQ2#rxq.rssi ->
                    remove_duplicates([{MAC, RxQ, PHYPayload} | Tail2], Unique);
                true -> % else
                    remove_duplicates([{MAC2, RxQ2, PHYPayload} | Tail2], Unique)
            end;
        false ->
            remove_duplicates(Tail, [{MAC, RxQ, PHYPayload} | Unique])
    end;
remove_duplicates([], Unique) ->
    Unique.

store_frame({MAC, RxQ, PHYPayload}, Dict) ->
    case dict:find(PHYPayload, Dict) of
        {ok, Frames} ->
            dict:store(PHYPayload, [{MAC, RxQ}|Frames], Dict);
        error ->
            {ok, Delay} = application:get_env(lorawan_server, deduplication_delay),
            {ok, _} = timer:send_after(Delay, {process, PHYPayload}),
            dict:store(PHYPayload, [{MAC, RxQ}], Dict)
    end.

% end of file
