%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
% Microchip LoRa(TM) Technology Mote
% http://www.microchip.com/Developmenttools/ProductDetails.aspx?PartNO=dm164138
%
-module(lorawan_application_microchip_mote).
-behaviour(lorawan_application).

-export([init/1, handle_join/3, handle_rx/4]).

-include_lib("lorawan_server_api/include/lorawan_application.hrl").

init(_App) ->
    ok.

handle_join(_Gateway, _Device, _Link) ->
    % accept any device
    ok.

% the data structure is explained in
% Lora_Legacy_Mote_Firmware/Includes/Board/MOTEapp.c:520
handle_rx(_Gateway, #link{devaddr=DevAddr},
        #rxdata{data= <<Light:5/binary, Temp:3/binary>>}, _RxQ) ->
    lager:debug("PUSH_DATA ~s ~s",[DevAddr, Light, Temp]),
    % display actual time
    {H, M, S} = time(),
    Time = lists:flatten(io_lib:format('~2..0b:~2..0b:~2..0b', [H, M, S])),
    {send, #txdata{port=2, data=list_to_binary(Time)}};

handle_rx(_Gateway, _Link, RxData, _RxQ) ->
    {error, {unexpected_data, RxData}}.

% end of file
