%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_gw_router).
-behaviour(gen_server).

-export([start_link/0]).
-export([register/3, status/2, uplinks/2, downlink/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([process_frame/3]).

-include_lib("lorawan_server_api/include/lorawan_application.hrl").
-include("lorawan.hrl").

-record(state, {pulladdr}).

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

register(MAC, Process, Target) ->
    gen_server:cast({global, ?MODULE}, {register, MAC, Process, Target}).

status(MAC, S) ->
    gen_server:cast({global, ?MODULE}, {status, MAC, S}).

uplinks(MAC, PkList) ->
    gen_server:cast({global, ?MODULE}, {uplinks, MAC, PkList}).

downlink(MAC, TxQ, PHYPayload) ->
    [Gateway] = mnesia:dirty_read(gateways, MAC),
    gen_server:cast({global, ?MODULE}, {downlink, MAC, TxQ, Gateway#gateway.tx_rfch, PHYPayload}).


init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{pulladdr=dict:new()}}.

handle_call(_Request, _From, State) ->
    {stop, {error, unknownmsg}, State}.

handle_cast({register, MAC, Process, Target}, #state{pulladdr=Dict}=State) ->
    case dict:find(MAC, Dict) of
        {ok, {Process, Target}} ->
            {noreply, State};
        _Else ->
            lager:info("Gateway ~w at ~w ~w", [MAC, Process, Target]),
            Dict2 = dict:store(MAC, {Process, Target}, Dict),
            {noreply, State#state{pulladdr=Dict2}}
    end;

handle_cast({status, MAC, S}, State) ->
    case lorawan_mac:process_status(MAC, S) of
        ok -> ok;
        {error, Error} ->
            lager:error("ERROR: ~w", [Error])
    end,
    {noreply, State};

handle_cast({uplinks, MAC, PkList}, State) ->
    % due to reflections the gateways may have received the same frame twice
    Unique = remove_duplicates(PkList, []),
    % handle the frames sequentially
    lists:foreach(
        fun({RxQ, PHYPayload}) ->
            spawn_link(?MODULE, process_frame, [MAC, RxQ, PHYPayload])
        end, Unique),
    {noreply, State};

handle_cast({downlink, MAC, TxQ, RFCh, PHYPayload}, #state{pulladdr=Dict}=State) ->
    case dict:find(MAC, Dict) of
        {ok, {Process, Target}} ->
            % send data to the gateway interface handler
            gen_server:cast(Process, {send, Target, TxQ, RFCh, PHYPayload});
        error ->
            lager:info("Downlink request ignored. Gateway ~w not connected.", [MAC])
    end,
    {noreply, State}.


% handler termination
handle_info({'EXIT', _FromPid, normal}, State) ->
    {noreply, State};
handle_info({'EXIT', _FromPid, Reason}, State) ->
    lager:error("Hanler terminated: ~w", [Reason]),
    {noreply, State}.

terminate(Reason, _State) ->
    lager:debug("gateway router terminated: ~w", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


remove_duplicates([{RxQ, PHYPayload} | Tail], Unique) ->
    % check if the first element is duplicate
    case lists:keytake(PHYPayload, 2, Tail) of
        {value, {RxQ2, PHYPayload}, Tail2} ->
            % select element of a better quality and re-check for other duplicates
            if
                RxQ#rxq.rssi >= RxQ2#rxq.rssi ->
                    remove_duplicates([{RxQ, PHYPayload} | Tail2], Unique);
                true -> % else
                    remove_duplicates([{RxQ2, PHYPayload} | Tail2], Unique)
            end;
        false ->
            remove_duplicates(Tail, [{RxQ, PHYPayload} | Unique])
    end;
remove_duplicates([], Unique) ->
    Unique.

process_frame(MAC, RxQ, PHYPayload) ->
    case lorawan_mac:process_frame(MAC, RxQ, PHYPayload) of
        ok -> ok;
        {send, TxQ, PHYPayload2} ->
            downlink(MAC, TxQ, PHYPayload2);
        {error, Error} ->
            lager:error("ERROR: ~w", [Error])
    end.

% end of file
