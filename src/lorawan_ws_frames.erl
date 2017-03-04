%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
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
    {ok, Timeout} = application:get_env(lorawan_server, websocket_timeout),
    init0(Req, Type, Format, #{idle_timeout => Timeout}).

init0(Req, <<"devices">>, Format, Opts) ->
    DevEUI = lorawan_mac:hex_to_binary(cowboy_req:binding(name, Req)),
    case mnesia:dirty_read(devices, DevEUI) of
        [] ->
            lager:warning("WebSocket for unknown DevEUI: ~w", [DevEUI]),
            Req2 = cowboy_req:reply(404, Req),
            {ok, Req2, undefined};
        [Dev] ->
            {cowboy_websocket, Req, #state{devaddr=Dev#device.link, format=Format}, Opts}
    end;
init0(Req, <<"links">>, Format, Opts) ->
    DevAddr = lorawan_mac:hex_to_binary(cowboy_req:binding(name, Req)),
    {cowboy_websocket, Req, #state{devaddr=DevAddr, format=Format}, Opts};
init0(Req, <<"groups">>, Format, Opts) ->
    GName = cowboy_req:binding(name, Req),
    {cowboy_websocket, Req, #state{gname=GName, format=Format}, Opts};
init0(Req, Unknown, _Format, _Opts) ->
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
    [lorawan_application_handler:store_frame(Link#link.devaddr, #txdata{data=Msg})
        || Link <- links(undefined, State)],
    {ok, State};
store_frame(Msg, #state{format=json} = State) ->
    case jsx:is_json(Msg) of
        true ->
            Struct = lorawan_admin:parse_admin(jsx:decode(Msg, [{labels, atom}])),
            DevAddr = proplists:get_value(devaddr, Struct),
            case proplists:get_value(time, Struct) of
                undefined ->
                    [lorawan_application_handler:store_frame(Link#link.devaddr, build_txdata(Struct))
                        || Link <- links(DevAddr, State)];
                Time ->
                    [lorawan_handler:downlink(Link, Time, build_txdata(Struct))
                        || Link <- links(DevAddr, State)]
            end,
            {ok, State};
        false ->
            lager:warning("JSON syntax error"),
            {stop, State}
    end.

build_txdata(Struct) ->
    #txdata{
        confirmed = proplists:get_value(confirmed, Struct, false),
        port = proplists:get_value(port, Struct),
        data = proplists:get_value(data, Struct, <<>>),
        pending = proplists:get_value(pending, Struct, false)
    }.

links(undefined, #state{devaddr=DevAddr, gname=undefined}) ->
    mnesia:dirty_read(links, DevAddr);
links(undefined, #state{gname=GName}) ->
    mnesia:dirty_select(links, [{#link{app= <<"websocket">>, appid=GName, _='_'}, [], ['$_']}]);
links(DevAddr, _State) ->
    mnesia:dirty_read(links, DevAddr).

websocket_info({send, _DevAddr, _AppArgs, #rxdata{data=Data}, _RxQ}, #state{format=raw} = State) ->
    {reply, {binary, Data}, State};
websocket_info({send, DevAddr, _AppArgs, RxData, RxQ}, #state{format=json} = State) ->
    Msg = lorawan_admin:build_admin([{devaddr, DevAddr}, {rxq, RxQ} | ?to_proplist(rxdata, RxData)]),
    {reply, {text, jsx:encode(Msg)}, State};
websocket_info(Info, State) ->
    lager:warning("Unknown info ~w", [Info]),
    {ok, State}.

get_processes(DevAddr, AppArgs) ->
    get_processes0({?MODULE, links, DevAddr}) ++ get_processes0({?MODULE, groups, AppArgs}).

get_processes0(Group) ->
    case pg2:get_members(Group) of
        List when is_list(List) -> List;
        {error, _} -> []
    end.

% end of file
