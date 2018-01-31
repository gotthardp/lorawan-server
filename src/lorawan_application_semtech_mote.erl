%
% Copyright (c) 2016-2018 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
% The 'Sensors GPS Demo' application from the Semtech LoRa Demo Kit
% https://github.com/Lora-net/LoRaMac-node
%
-module(lorawan_application_semtech_mote).
-behaviour(lorawan_application).

-export([init/1, handle_join/3, handle_uplink/4, handle_rxq/5, handle_delivery/3]).

-include("lorawan.hrl").
-include("lorawan_db.hrl").

init(_App) ->
    ok.

handle_join({_Network, _Profile, _Device}, {_MAC, _RxQ}, _DevAddr) ->
    % accept any device
    ok.

handle_uplink({_Network, _Profile, _Node}, _RxQ, {missed, _Receipt}, _Frame) ->
    retransmit;
handle_uplink(_Context, _RxQ, _LastMissed, _Frame) ->
    % accept and wait for deduplication
    {ok, []}.

% the data structure is explained in
% https://github.com/Lora-net/LoRaMac-node/blob/master/src/apps/LoRaMac/classA/LoRaMote/main.c#L207
handle_rxq({_Network, _Profile, #node{devaddr=DevAddr}}, _Gateways, _WillReply,
        #frame{port=2, data= <<LED, Press:16, Temp:16, _AltBar:16, Batt, _Lat:24, _Lon:24, _AltGps:16>>}, []) ->
    lager:debug("PUSH_DATA ~w ~w ~w ~w",[DevAddr, Press, Temp, Batt]),
    % blink with the LED indicator
    {send, #txdata{port=2, data= <<((LED+1) rem 2)>>}};

handle_rxq(_Context, _Gateways, _WillReply, #frame{port=Port, data=Data}, []) ->
    {error, {wrong_data, {Port, Data}}}.

handle_delivery({_Network, _Profile, _Node}, _Result, _Receipt) ->
    ok.

% end of file
