%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_handler).

-export([handle_rxpk/3]).
-export([rxpk/3]).

handle_rxpk(MAC, RxQ, PHYPayload) ->
    Pid = spawn_link(?MODULE, rxpk, [MAC, RxQ, PHYPayload]),
    {ok, Pid}.

rxpk(MAC, RxQ, PHYPayload) ->
    case lorawan_mac:process_frame(MAC, RxQ, PHYPayload) of
        ok -> ok;
        {send, Gateway, TxQ, PHYPayload2} ->
            lorawan_iface_forwarder:txsend(Gateway, TxQ, PHYPayload2);
        {error, Error} ->
            lager:error("ERROR: ~w", [Error])
    end.

% end of file
