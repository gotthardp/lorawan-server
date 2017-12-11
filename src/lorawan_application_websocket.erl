%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_application_websocket).
-behaviour(lorawan_application).

-export([init/1, handle_join/3, handle_uplink/4, handle_rxq/4]).

-include("lorawan_application.hrl").

init(_App) ->
    {ok, [
        {"/ws/:type/:name/raw", lorawan_ws_frames, [<<"raw">>]},
        {"/ws/:type/:name/json", lorawan_ws_frames, [<<"json">>]},
        {"/ws/:type/:name/www-form", lorawan_ws_frames, [<<"www-form">>]}
    ]}.

handle_join({Network, Profile, Device}, {MAC, RxQ}, DevAddr) ->
    % accept any device
    ok.

handle_uplink({Network, Profile, Node}, {MAC, RxQ}, {lost, State}, Frame) ->
    retransmit;
handle_uplink({Network, Profile, Node}, {MAC, RxQ}, _LastAcked, Frame) ->
    % accept any device
    ok.

handle_rxq({Network, Profile, Node}, Gateways, Frame, State) ->
    % accept any device
    ok.

handle_rx(Gateway, #node{devaddr=DevAddr}=Link, #rxdata{port=Port} = RxData, RxQ) ->
    case send_to_sockets(Gateway, Link, RxData, RxQ) of
        [] -> lager:warning("Frame not sent to any process");
        _List -> ok
    end,
    lorawan_handler:send_stored_frames(DevAddr, Port).

send_to_sockets(Gateway, #node{devaddr=DevAddr, appid=AppID}=Link, RxData, RxQ) ->
    Sockets = lorawan_ws_frames:get_processes(DevAddr, AppID),
    [Pid ! {send, Gateway, Link, RxData, RxQ} || Pid <- Sockets].

% end of file
