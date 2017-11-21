%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
% The 'Sensors GPS Demo' application from the Semtech LoRa Demo Kit
% https://github.com/Lora-net/LoRaMac-node
%
-module(lorawan_application_semtech_mote).
-behaviour(lorawan_application).

-export([init/1, handle_join/3, handle_rx/4]).

-include_lib("lorawan_server_api/include/lorawan_application.hrl").

init(_App) ->
    ok.

handle_join(_Gateway, _Device, _Link) ->
    % accept any device
    ok.

% the data structure is explained in
% https://github.com/Lora-net/LoRaMac-node/blob/master/src/apps/LoRaMac/classA/LoRaMote/main.c#L207
handle_rx(_Gateway, #link{devaddr=DevAddr},
        #rxdata{port=2, data= <<LED, Press:16, Temp:16, _AltBar:16, Batt, _Lat:24, _Lon:24, _AltGps:16>>}, _RxQ) ->
    lager:debug("PUSH_DATA ~w ~w ~w ~w",[DevAddr, Press, Temp, Batt]),
    % blink with the LED indicator
    {send, #txdata{port=2, data= <<((LED+1) rem 2)>>}};

handle_rx(_Gateway, _Link, RxData, _RxQ) ->
    {error, {unexpected_data, RxData}}.

% end of file
