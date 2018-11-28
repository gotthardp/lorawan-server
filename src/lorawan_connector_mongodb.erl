%
% Copyright (c) 2016-2018 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_connector_mongodb).
-behaviour(gen_server).

-export([start_connector/1, stop_connector/1]).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("lorawan_db.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-record(state, {conn, pool, publish_uplinks, publish_events}).

start_connector(#connector{connid=Id}=Connector) ->
    lorawan_connector_sup:start_child({mongodb, Id}, ?MODULE, [Connector]).

stop_connector(Id) ->
    lorawan_connector_sup:stop_child({mongodb, Id}).

start_link(Connector) ->
    gen_server:start_link(?MODULE, [Connector], []).

init([#connector{connid=Id, app=App, uri= <<"mongodb://", Servers0/binary>>,
        publish_uplinks=PubUp, publish_events=PubEv}=Connector]) ->
    process_flag(trap_exit, true),
    ok = pg2:join({backend, App}, self()),
    lager:debug("Connecting ~s to mongodb ~s", [Id, Servers0]),
    Pool = binary_to_atom(Id, latin1),
    % connect
    {UserName, Password} = credentials(Connector),
    mongodb:replicaSets(Pool, 10,
        string:tokens(binary_to_list(Servers0), ", "), UserName, Password),
    mongodb:connect(Pool),
    try
        {ok, #state{
            conn=Connector,
            pool=Pool,
            publish_uplinks=lorawan_connector:prepare_filling(PubUp),
            publish_events=lorawan_connector:prepare_filling(PubEv)
        }}
    catch
        _:Error ->
            lorawan_connector:raise_failed(Id, Error),
            {stop, shutdown}
    end.

credentials(#connector{name=UserName})
        when UserName == undefined; UserName == <<>> ->
    {undefined, undefined};
credentials(#connector{name=UserName, pass=Password}) ->
    {UserName, Password}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknownmsg}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(nodes_changed, State) ->
    % nothing to do here
    {noreply, State};

handle_info({uplink, _Node, Vars0}, #state{pool=Pool, publish_uplinks=PatPub}=State) ->
    store_fields(Pool, PatPub, Vars0),
    {noreply, State};

handle_info({event, _Node, Vars0}, #state{pool=Pool, publish_events=PatPub}=State) ->
    store_fields(Pool, PatPub, Vars0),
    {noreply, State};

handle_info({status, From}, #state{conn=#connector{connid=Id, app=App, uri=Uri}}=State) ->
    From ! {status, [
        #{module => <<"mongodb">>, pid => lorawan_connector:pid_to_binary(self()),
            connid => Id, app => App, uri => Uri, status => get_status(State)}]},
    {noreply, State};

handle_info(Unknown, State) ->
    lager:debug("Unknown message: ~p", [Unknown]),
    {noreply, State}.

terminate(Reason, #state{conn=Connector, pool=Pool}) ->
    log_termination(Reason, Connector),
    mongodb:deleteConnection(Pool),
    ok.

log_termination(Reason, #connector{connid=ConnId})
        when Reason == normal; Reason == shutdown ->
    lager:debug("Connector ~s terminated: ~p", [ConnId, Reason]);
log_termination(Reason, #connector{connid=ConnId}) ->
    lager:warning("Connector ~s terminated: ~p", [ConnId, Reason]).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


store_fields(_Pool, Pattern, _Vars0) when Pattern == undefined; Pattern == ?EMPTY_PATTERN ->
    ok;
store_fields(Pool, Pattern, Vars0) ->
    Target = lorawan_connector:fill_pattern(Pattern, lorawan_admin:build(Vars0)),
    {Database, Collection} =
        case binary:split(Target, <<$/>>) of
            [DB, CN | _] ->
                {DB, CN};
            [CN] ->
                {<<"local">>, CN}
        end,
    Mong = mongoapi:new(Pool, Database),
    Mong:createCollection(Collection),
    {ok, _} = Mong:save(Collection, prepare_bson(Vars0)).

prepare_bson(Data) ->
    maps:map(
        fun
            (Key, Value) when Key == mac; Key == deveui; Key == devaddr; Key == data ->
                lorawan_utils:binary_to_hex(Value);
            (_Key, Value) when is_map(Value) ->
                prepare_bson(Value);
            (_Key, Value) ->
                Value
        end,
        maps:filter(
            fun
                (_Key, undefined) -> false;
                (_Key, _Value) -> true
            end,
            Data)).

get_status(#state{pool=Pool}) ->
    case mongodb:is_connected(Pool) of
        true -> <<"connected">>;
        false -> <<"disconnected">>
    end.

% end of file
