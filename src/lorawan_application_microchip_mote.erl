%
% Copyright (c) 2016 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
% Microchip LoRa(TM) Technology Mote
% http://www.microchip.com/Developmenttools/ProductDetails.aspx?PartNO=dm164138
%
-module(lorawan_application_microchip_mote).
-behaviour(lorawan_application).

-export([init/1, handle_join/3, handle_rx/5]).

init(_App) ->
    ok.

handle_join(_DevAddr, _App, _AppID) ->
    % accept any device
    ok.

% the data structure is explained in
% Lora_Legacy_Mote_Firmware/Includes/Board/MOTEapp.c:520
handle_rx(DevAddr, _App, _AppID, _Port, <<Light:5/binary, Temp:3/binary>>) ->
    lager:debug("PUSH_DATA ~s ~s",[DevAddr, Light, Temp]),
    % display actual time
    {H, M, S} = time(),
    Time = lists:flatten(io_lib:format('~2..0b:~2..0b:~2..0b', [H, M, S])),
    {send, 2, list_to_binary(Time)};

handle_rx(_DevAddr, _App, _AppID, Port, Data) ->
    {error, {unexpected_data, Port, Data}}.

% end of file
