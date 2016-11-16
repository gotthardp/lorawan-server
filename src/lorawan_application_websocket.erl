%
% Copyright (c) 2016 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_application_websocket).
-behaviour(lorawan_application).

-export([init/1, handle_join/3, handle_rx/5]).

init(_App) ->
    {ok, [
        {"/ws/:deveui/raw", lorawan_ws_raw, []}
    ]}.

handle_join(_DevAddr, _App, _AppID) ->
    % accept any device
    ok.

handle_rx(DevAddr, _App, AppID, Port, Data) ->
    Sockets = lorawan_ws_raw:get_processes(DevAddr),
    [Pid ! {send, DevAddr, AppID, Port, Data} || Pid <- Sockets],
    lorawan_application:send_stored_frames(DevAddr, Port).

% end of file
