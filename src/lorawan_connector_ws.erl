%
% Copyright (c) 2016-2018 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_connector_ws).

-export([start_connector/1, stop_connector/1]).
-export([init/2]).
-export([websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

-include("lorawan_db.hrl").

-record(state, {connector, bindings}).

start_connector(#connector{connid=Id, uri= <<"ws:", URI/binary>>}=Connector) ->
    % convert our pattern to cowboy pattern
    URI2 = re:replace(URI, "{([^}])}", ":\\1", [{return, binary}]),
    lorawan_http_registry:update_routes({ws, Id}, [
        {URI2, ?MODULE, [Connector]}
    ]).

stop_connector(Id) ->
    lorawan_http_registry:delete_routes({ws, Id}).

init(Req, [#connector{connid=Id}=Connector]) ->
    Bindings = cowboy_req:bindings(Req),
    case validate(maps:to_list(Bindings)) of
        ok ->
            {ok, Timeout} = application:get_env(lorawan_server, websocket_timeout),
            {cowboy_websocket, Req, #state{connector=Connector, bindings=Bindings}, #{idle_timeout => Timeout}};
        {error, Error} ->
            lorawan_utils:throw_error({connector, Id}, Error),
            Req2 = cowboy_req:reply(404, Req),
            {ok, Req2, undefined}
    end.

validate([{Key, Value} | Other]) ->
    case validate0(Key, Value) of
        ok ->
            validate(Other);
        Else ->
            Else
    end;
validate([])->
    ok.

validate0(deveui, DevEUI) ->
    case mnesia:dirty_read(devices, lorawan_utils:hex_to_binary(DevEUI)) of
        [#device{}] ->
            ok;
        _Else ->
            {error, {unknown_deveui, DevEUI}}
    end;
validate0(devaddr, DevAddr) ->
    case mnesia:dirty_read(nodes, lorawan_utils:hex_to_binary(DevAddr)) of
        [#node{}] ->
            ok;
        _Else ->
            {error, {unknown_devaddr, DevAddr}}
    end.

websocket_init(#state{connector=#connector{connid=Id, app=App}, bindings=Bindings} = State) ->
    lager:debug("WebSocket connector ~p with ~p", [Id, Bindings]),
    ok = pg2:join({backend, App}, self()),
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

handle_downlink(Msg, #state{connector=Connector, bindings=Bindings}=State) ->
    case lorawan_connector:decode_and_downlink(Connector, Msg, Bindings) of
        ok ->
            {ok, State};
        {error, {Object, Error}} ->
            lorawan_utils:throw_error(Object, Error),
            {stop, State};
        {error, Error} ->
            lorawan_utils:throw_error({connector, Connector#connector.connid}, Error),
            {stop, State}
    end.

websocket_info({uplink, _Node, Vars},
        #state{connector=#connector{format=Format}, bindings=Bindings} = State) ->
    case lorawan_connector:same_common_vars(Vars, Bindings) of
        true ->
            {reply, encode_uplink(Format, Vars), State};
        false ->
            {ok, State}
    end;
websocket_info(Info, State) ->
    lager:warning("Unknown info ~p", [Info]),
    {ok, State}.

encode_uplink(<<"raw">>, Vars) ->
    {binary, maps:get(data, Vars, <<>>)};
encode_uplink(<<"json">>, Vars) ->
    {text, jsx:encode(lorawan_admin:build(Vars))};
encode_uplink(<<"www-form">>, Vars) ->
    {text, lorawan_connector:form_encode(Vars)}.

terminate(Reason, _Req, _State) ->
    lager:debug("WebSocket terminated: ~p", [Reason]),
    ok.

% end of file
