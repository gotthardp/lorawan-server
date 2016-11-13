%
% Copyright (c) 2016 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_handler).

-export([handle_rxpk/4]).
-export([rxpk/5]).

handle_rxpk(MAC, RxQ, RF, PHYPayload) ->
    Pid = spawn_link(?MODULE, rxpk, [self(), MAC, RxQ, RF, PHYPayload]),
    {ok, Pid}.

rxpk(Caller, MAC, RxQ, RF, PHYPayload) ->
    case lorawan_mac:process_frame(MAC, RxQ, RF, PHYPayload) of
        ok -> ok;
        {send, Time2, RF2, PHYPayload2} ->
            lorawan_iface_forwarder:txsend(Caller, MAC, Time2, RF2, PHYPayload2);
        {error, Error} ->
            lager:error("ERROR: ~w", [Error])
    end.

% end of file
