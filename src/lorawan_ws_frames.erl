%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_ws_frames).

-export([init/2]).
-export([websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).
-export([get_processes/2]).

-include_lib("lorawan_server_api/include/lorawan_application.hrl").
-include("lorawan.hrl").

-record(state, {target, format}).

init(Req, [Format]) ->
    Type = cowboy_req:binding(type, Req),
    {ok, Timeout} = application:get_env(lorawan_server, websocket_timeout),
    init0(Req, Type, Format, #{idle_timeout => Timeout}).

init0(Req, <<"devices">>, Format, Opts) ->
    DevEUI = lorawan_mac:hex_to_binary(cowboy_req:binding(name, Req)),
    case mnesia:dirty_read(devices, DevEUI) of
        [Dev=#device{app= <<"websocket">>}] ->
            {cowboy_websocket, Req, #state{target=#{devaddr=>Dev#device.link}, format=Format}, Opts};
        _Else ->
            lager:warning("No WebSocket for DevEUI: ~w", [DevEUI]),
            Req2 = cowboy_req:reply(404, Req),
            {ok, Req2, undefined}
    end;
init0(Req, <<"nodes">>, Format, Opts) ->
    DevAddr = lorawan_mac:hex_to_binary(cowboy_req:binding(name, Req)),
    case mnesia:dirty_read(links, DevAddr) of
        [#link{app= <<"websocket">>}] ->
            {cowboy_websocket, Req, #state{target=#{devaddr=>DevAddr}, format=Format}, Opts};
        _Else ->
            lager:warning("No WebSocket for DevAddr: ~w", [DevAddr]),
            Req2 = cowboy_req:reply(404, Req),
            {ok, Req2, undefined}
    end;
init0(Req, <<"groups">>, Format, Opts) ->
    AppID = cowboy_req:binding(name, Req),
    {cowboy_websocket, Req, #state{target=#{group=>AppID}, format=Format}, Opts};
init0(Req, Unknown, _Format, _Opts) ->
    lager:warning("Unknown WebSocket type: ~s", [Unknown]),
    Req2 = cowboy_req:reply(404, Req),
    {ok, Req2, undefined}.

websocket_init(#state{target=#{devaddr:=DevAddr}} = State) ->
    lager:debug("WebSocket to node ~w", [DevAddr]),
    ok = pg2:create({?MODULE, nodes, DevAddr}),
    ok = pg2:join({?MODULE, nodes, DevAddr}, self()),
    {ok, State};
websocket_init(#state{target=#{group:=AppID}} = State) ->
    lager:debug("WebSocket to group '~s'", [AppID]),
    ok = pg2:create({?MODULE, groups, AppID}),
    ok = pg2:join({?MODULE, groups, AppID}, self()),
    {ok, State}.

websocket_handle({text, Msg}, State) ->
    handle_downlink(Msg, State);
websocket_handle({binary, Msg}, State) ->
    handle_downlink(Msg, State);
websocket_handle({ping, _}, State) ->
    % no action needed as server handles pings automatically
    {ok, State};
websocket_handle(Data, State) ->
    lager:warning("Unknown handle ~w", [Data]),
    {ok, State}.

handle_downlink(Msg, #state{target=Target, format=Format} = State) ->
    case lorawan_application_backend:handle_downlink(Msg, Format, Target) of
        ok ->
            {ok, State};
        {error, {Object, Error}} ->
            lorawan_utils:throw_error(Object, Error),
            {stop, State};
        {error, Error} ->
            lorawan_utils:throw_error(server, Error),
            {stop, State}
    end.

websocket_info({send, Gateway, #link{appid=AppID}=Link, RxData, RxQ}, #state{format=Format} = State) ->
    Handler =
        case mnesia:dirty_read(handlers, AppID) of
            [Rec] -> Rec#handler{format=Format};
            [] -> #handler{format=Format}
        end,
    case lorawan_application_backend:parse_uplink(Handler, Gateway, Link, RxData, RxQ) of
        {<<"application/octet-stream">>, Data, _Vars} ->
            {reply, {binary, Data}, State};
        {_Else, Data, _Vars} ->
            {reply, {text, Data}, State}
    end;
websocket_info(Info, State) ->
    lager:warning("Unknown info ~p", [Info]),
    {ok, State}.

terminate(Reason, _Req, _State) ->
    lager:debug("WebSocket terminated: ~p", [Reason]),
    ok.

get_processes(DevAddr, AppID) ->
    get_processes0({?MODULE, nodes, DevAddr}) ++ get_processes0({?MODULE, groups, AppID}).

get_processes0(Group) ->
    case pg2:get_members(Group) of
        List when is_list(List) -> List;
        {error, _} -> []
    end.

% end of file
