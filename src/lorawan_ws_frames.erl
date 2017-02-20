%
% Copyright (c) 2016 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_ws_frames).

-export([init/2]).
-export([websocket_init/1, websocket_handle/2, websocket_info/2]).
-export([get_processes/2]).

-include_lib("lorawan_server_api/include/lorawan_application.hrl").
-include("lorawan.hrl").
-record(state, {devaddr, gname, format}).

init(Req, [Format]) ->
    Type = cowboy_req:binding(type, Req),
    init0(Req, Type, Format).

init0(Req, <<"devices">>, Format) ->
    DevEUI = lorawan_mac:hex_to_binary(cowboy_req:binding(name, Req)),
    case mnesia:dirty_read(devices, DevEUI) of
        [] ->
            lager:warning("WebSocket for unknown DevEUI: ~w", [DevEUI]),
            Req2 = cowboy_req:reply(404, Req),
            {ok, Req2, undefined};
        [Dev] ->
            {cowboy_websocket, Req, #state{devaddr=Dev#device.link, format=Format}}
    end;
init0(Req, <<"links">>, Format) ->
    DevAddr = lorawan_mac:hex_to_binary(cowboy_req:binding(name, Req)),
    {cowboy_websocket, Req, #state{devaddr=DevAddr, format=Format}};
init0(Req, <<"groups">>, Format) ->
    GName = cowboy_req:binding(name, Req),
    {cowboy_websocket, Req, #state{gname=GName, format=Format}};
init0(Req, Unknown, _Format) ->
    lager:warning("Unknown WebSocket type: ~s", [Unknown]),
    Req2 = cowboy_req:reply(404, Req),
    {ok, Req2, undefined}.

websocket_init(#state{devaddr=DevAddr, gname=undefined} = State) ->
    lager:debug("WebSocket to link ~w", [DevAddr]),
    ok = pg2:create({?MODULE, links, DevAddr}),
    ok = pg2:join({?MODULE, links, DevAddr}, self()),
    {ok, State};
websocket_init(#state{gname=GName} = State) ->
    lager:debug("WebSocket to group '~s'", [GName]),
    ok = pg2:create({?MODULE, groups, GName}),
    ok = pg2:join({?MODULE, groups, GName}, self()),
    {ok, State}.

websocket_handle({text, Msg}, State) ->
    store_frame(Msg, State);
websocket_handle({binary, Msg}, State) ->
    store_frame(Msg, State);
websocket_handle({ping, _}, State) ->
    % no action needed as server handles pings automatically
    {ok, State};
websocket_handle(Data, State) ->
    lager:warning("Unknown handle ~w", [Data]),
    {ok, State}.

store_frame(Msg, #state{format=raw} = State) ->
    store_frame0(undefined, undefined, Msg, State);
store_frame(Msg, #state{format=json} = State) ->
    Struct = jsx:decode(Msg, [{labels, atom}]),
    DevAddr = case proplists:get_value(devaddr, Struct) of
        undefined -> undefined;
        Hex1 -> lorawan_mac:hex_to_binary(Hex1)
    end,
    Port = proplists:get_value(port, Struct),
    Data = case proplists:get_value(data, Struct) of
        undefined -> <<>>;
        Hex2 -> lorawan_mac:hex_to_binary(Hex2)
    end,
    store_frame0(DevAddr, Port, Data, State).

store_frame0(undefined, Port, Data, #state{devaddr=DevAddr, gname=undefined} = State) ->
    lorawan_application_handler:store_frame(DevAddr, #txdata{port=Port, data=Data}),
    {ok, State};
store_frame0(undefined, Port, Data, #state{gname=GName} = State) ->
    [lorawan_application_handler:store_frame(DevAddr, #txdata{port=Port, data=Data})
        || DevAddr <- mnesia:dirty_select(links, [{#link{devaddr='$1', app= <<"websocket">>, appid=GName, _='_'}, [], ['$1']}])],
    {ok, State};
store_frame0(DevAddr, Port, Data, State) ->
    lorawan_application_handler:store_frame(DevAddr, #txdata{port=Port, data=Data}),
    {ok, State}.

websocket_info({send, _DevAddr, _AppID, #rxdata{data=Data}}, #state{format=raw} = State) ->
    {reply, {binary, Data}, State};
websocket_info({send, DevAddr, _AppID, #rxdata{port=Port, data=Data}}, #state{format=json} = State) ->
    Msg = [{devaddr, lorawan_mac:binary_to_hex(DevAddr)}, {port, Port}, {data, lorawan_mac:binary_to_hex(Data)}],
    {reply, {text, jsx:encode(Msg)}, State};
websocket_info(Info, State) ->
    lager:warning("Unknown info ~w", [Info]),
    {ok, State}.

get_processes(DevAddr, AppID) ->
    get_processes0({?MODULE, links, DevAddr}) ++ get_processes0({?MODULE, groups, AppID}).

get_processes0(Group) ->
    case pg2:get_members(Group) of
        List when is_list(List) -> List;
        {error, _} -> []
    end.

% end of file
