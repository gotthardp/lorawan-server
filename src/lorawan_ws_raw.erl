%
% Copyright (c) 2016 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_ws_raw).

-export([init/2]).
-export([websocket_init/1, websocket_handle/2, websocket_info/2]).
-export([get_processes/1]).

-include_lib("lorawan_server_api/include/lorawan_application.hrl").
-include("lorawan.hrl").
-record(state, {deveui, devaddr}).

init(Req, _Opts) ->
    DevEUI = lorawan_mac:hex_to_binary(cowboy_req:binding(deveui, Req)),
    case mnesia:dirty_read(devices, DevEUI) of
        [] ->
            lager:warning("WebSocket for unknown DevEUI: ~w", [DevEUI]),
            Req2 = cowboy_req:reply(404, Req),
            {ok, Req2, undefined};
        [Dev] ->
            {cowboy_websocket, Req, #state{deveui=DevEUI, devaddr=Dev#device.link}}
    end.

websocket_init(#state{devaddr=DevAddr} = State) ->
    ok = pg2:create({?MODULE, DevAddr}),
    ok = pg2:join({?MODULE, DevAddr}, self()),
    {ok, State}.

websocket_handle({text, Msg}, State) ->
    store_frame(undefined, Msg, State);
websocket_handle({binary, Msg}, State) ->
    store_frame(undefined, Msg, State);
websocket_handle(Data, State) ->
    lager:warning("Unknown handle ~w", [Data]),
    {ok, State}.

store_frame(Port, Data, #state{devaddr=DevAddr} = State) ->
    lorawan_application_handler:store_frame(DevAddr, #txdata{port=Port, data=Data}),
    {ok, State}.

websocket_info({send, _DevAddr, _AppID, #rxdata{data=Data}}, State) ->
    {reply, {binary, Data}, State};
websocket_info(Info, State) ->
    lager:warning("Unknown info ~w", [Info]),
    {ok, State}.

get_processes(DevAddr) ->
    case pg2:get_members({?MODULE, DevAddr}) of
        List when is_list(List) -> List;
        {error, _} -> []
    end.

% end of file
