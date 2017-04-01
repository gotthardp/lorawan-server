%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_application_connector).
-behaviour(lorawan_application).

-export([init/1, handle_join/3, handle_rx/5]).

-include_lib("lorawan_server_api/include/lorawan_application.hrl").
-include("lorawan.hrl").

init(_App) ->
    ok.

handle_join(_DevAddr, _AppID, _AppArgs) ->
    % accept any device
    ok.

handle_rx(_DevAddr, _AppID, _AppArgs, #rxdata{last_lost=true}, _RxQ) ->
    retransmit;
handle_rx(DevAddr, AppID, AppArgs, #rxdata{port=Port} = RxData, RxQ) ->
    send_to_backend(DevAddr, AppID, AppArgs, RxData, RxQ),
    lorawan_handler:send_stored_frames(DevAddr, Port).

send_to_backend(_DevAddr, AppID, _AppArgs, RxData, _RxQ) ->
    case mnesia:dirty_read(handlers, AppID) of
        [Handler] ->
            {ok, Pid} = lorawan_connector_factory:get_or_init_connector(Handler#handler.connid),
            gen_server:cast(Pid, {publish, Handler#handler.outbound, RxData#rxdata.data});
        [] ->
            {error, {unknown_appid, AppID}}
    end.

% end of file
