%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_application_websocket).
-behaviour(lorawan_application).

-export([init/1, handle_join/3, handle_rx/5]).

-include_lib("lorawan_server_api/include/lorawan_application.hrl").

init(_App) ->
    {ok, [
        {"/ws/:type/:name/raw", lorawan_ws_frames, [<<"raw">>]},
        {"/ws/:type/:name/json", lorawan_ws_frames, [<<"json">>]}
    ]}.

handle_join(_DevAddr, _AppID, _AppArgs) ->
    % accept any device
    ok.

handle_rx(_DevAddr, _AppID, _AppArgs, #rxdata{last_lost=true}, _RxQ) ->
    retransmit;
handle_rx(DevAddr, AppID, AppArgs, #rxdata{port=Port} = RxData, RxQ) ->
    case send_to_sockets(DevAddr, AppID, AppArgs, RxData, RxQ) of
        [] -> lager:warning("Frame not sent to any process");
        _List -> ok
    end,
    lorawan_handler:send_stored_frames(DevAddr, Port).

send_to_sockets(DevAddr, AppID, AppArgs, RxData, RxQ) ->
    Sockets = lorawan_ws_frames:get_processes(DevAddr, AppID),
    [Pid ! {send, DevAddr, AppID, AppArgs, RxData, RxQ} || Pid <- Sockets].

% end of file
