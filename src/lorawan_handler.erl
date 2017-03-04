%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_handler).

-export([handle_rxpk/3, downlink/3]).
-export([rxpk/3]).

-include_lib("lorawan_server_api/include/lorawan_application.hrl").

handle_rxpk(MAC, RxQ, PHYPayload) ->
    Pid = spawn_link(?MODULE, rxpk, [MAC, RxQ, PHYPayload]),
    {ok, Pid}.

rxpk(MAC, RxQ, PHYPayload) ->
    case lorawan_mac:process_frame(MAC, RxQ, PHYPayload) of
        ok -> ok;
        {send, TxQ, PHYPayload2} ->
            lorawan_iface_forwarder:txsend(MAC, TxQ, PHYPayload2);
        {error, Error} ->
            lager:error("ERROR: ~w", [Error])
    end.

downlink(Link, Time, TxData) ->
    case lorawan_mac:handle_downlink(Link, Time, TxData) of
        {send, TxQ, PHYPayload2} ->
            lorawan_iface_forwarder:txsend(Link#link.last_mac, TxQ, PHYPayload2);
        {error, Error} ->
            lager:error("ERROR: ~w", [Error])
    end.

% end of file
