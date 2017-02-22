%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_application_websocket).
-behaviour(lorawan_application).

-export([init/1, handle_join/3, handle_rx/4]).

-include_lib("lorawan_server_api/include/lorawan_application.hrl").

init(_App) ->
    {ok, [
        {"/ws/:type/:name/raw", lorawan_ws_frames, [raw]},
        {"/ws/:type/:name/json", lorawan_ws_frames, [json]}
    ]}.

handle_join(_DevAddr, _App, _AppID) ->
    % accept any device
    ok.

handle_rx(DevAddr, _App, AppID, #rxdata{last_lost=true} = RxData) ->
    send_to_sockets(DevAddr, AppID, RxData),
    retransmit;
handle_rx(DevAddr, _App, AppID, #rxdata{port=Port} = RxData) ->
    send_to_sockets(DevAddr, AppID, RxData),
    lorawan_application_handler:send_stored_frames(DevAddr, Port).

send_to_sockets(DevAddr, AppID, RxData) ->
    Sockets = lorawan_ws_frames:get_processes(DevAddr, AppID),
    [Pid ! {send, DevAddr, AppID, RxData} || Pid <- Sockets].

% end of file
