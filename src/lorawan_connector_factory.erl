%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_connector_factory).
-behaviour(gen_server).

-export([start_link/0, publish/2, disable_connector/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("lorawan.hrl").

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

publish(ConnId, Message) ->
    case ensure_connected(ConnId) of
        {ok, Pid} ->
            gen_server:cast(Pid, {publish, Message});
        {error, Error} ->
            {error, Error}
    end.

ensure_connected(ConnId) ->
    gen_server:call({global, ?MODULE}, {ensure_connected, ConnId}).

init([]) ->
    % start connectors that need to subscribe
    lists:foreach(
        fun(Conn) ->
            init_connector(Conn)
        end,
        mnesia:dirty_select(connectors,
            [{#connector{enabled='$1', subscribe='$2', _='_'},
             [{'==', '$1', true}, {'=/=', '$2', undefined}, {'=/=', '$2', <<>>}], ['$_']}])),
    {ok, _} = mnesia:subscribe({table, connectors, simple}),
    {ok, undefined}.

handle_call({ensure_connected, ConnId}, _From, State) ->
    {reply, get_or_init_connector_id(ConnId), State};
handle_call({ensure_disconnected, ConnId}, _From, State) ->
    {reply, maybe_terminate_connector_id(ConnId), State};
handle_call(_Request, _From, State) ->
    {stop, {error, unknownmsg}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({mnesia_table_event, {write, Record0, _Trans}}, State) ->
    case setelement(1, Record0, connector) of
        COn when COn#connector.enabled==true, COn#connector.subscribe =/= undefined ->
            get_or_init_subscription(COn);
        COff ->
            maybe_terminate_connector_id(COff#connector.connid)
    end,
    {noreply, State};
handle_info({mnesia_table_event, {delete, {connectors, ConnId}, _Trans}}, State) ->
    maybe_terminate_connector_id(ConnId),
    {noreply, State};

handle_info(Info, State) ->
    lager:debug("unknown info ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


get_or_init_connector_id(ConnId) ->
    case syn:find_by_key(ConnId) of
        undefined ->
            init_connector_id(ConnId);
        Pid ->
            {ok, Pid}
    end.

init_connector_id(ConnId) ->
    case mnesia:dirty_read(connectors, ConnId) of
        [#connector{enabled=false}] ->
            {error, {connector_disabled, ConnId}};
        [Conn] ->
            init_connector(Conn);
        [] ->
            {error, {unknown_connector, ConnId}}
    end.

get_or_init_subscription(#connector{connid=ConnId, enabled=false}) ->
    {error, {connector_disabled, ConnId}};
get_or_init_subscription(#connector{connid=ConnId, subscribe=Topic} = Conn) ->
    case syn:find_by_key(ConnId) of
        undefined ->
            init_connector(Conn);
        Pid ->
            gen_server:call(Pid, {resubscribe, Topic}),
            {ok, Pid}
    end.

init_connector(Conn) ->
    case parse_uri(Conn#connector.uri) of
        {ok, {http, _, _, _, _, _} = ConnUri} ->
            lorawan_connector_sup_http:start_child(ConnUri, Conn);
        {ok, {https, _, _, _, _, _} = ConnUri} ->
            lorawan_connector_sup_http:start_child(ConnUri, Conn);
        {ok, {mqtt, _, _, _, _, _} = ConnUri} ->
            lorawan_connector_sup_mqtt:start_child(ConnUri, Conn);
        {ok, {mqtts, _, _, _, _, _} = ConnUri} ->
            lorawan_connector_sup_mqtt:start_child(ConnUri, Conn);
        {ok, {Other, _, _, _, _, _}} ->
            {error, {unknown_scheme, Other}};
        {error, Error} ->
            {error, Error}
    end.

maybe_terminate_connector_id(ConnId) ->
    case syn:find_by_key(ConnId) of
        undefined ->
            ok;
        Pid ->
            gen_server:cast(Pid, disconnect)
    end.

disable_connector(ConnId) ->
    [Conn] = mnesia:dirty_read(connectors, ConnId),
    ok = mnesia:dirty_write(connectors, Conn#connector{enabled=false}).

parse_uri(Uri) when is_binary(Uri) ->
    parse_uri(binary_to_list(Uri));

parse_uri(Uri) when is_list(Uri) ->
    http_uri:parse(Uri, [{scheme_defaults, [{http, 80}, {https, 443},
        {mqtt, 1883}, {mqtts, 8883}]}]).

% end of file
