%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_worker).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, undefined}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({Req, MAC, RxQ, PHYPayload}, State) ->
    case catch lorawan_mac:process_frame(MAC, RxQ, PHYPayload) of
        ok -> ok;
        {send, DevAddr, TxQ, PHYPayload2} ->
            lorawan_gw_router:downlink(Req, MAC, DevAddr, TxQ, PHYPayload2);
        {error, {Object, Error}} ->
            lorawan_utils:throw_error(Object, Error);
        {error, Error} ->
            lorawan_utils:throw_error(server, Error);
        {'EXIT', Error} ->
            lager:error("~p", [Error])
    end,
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% end of file
