%
%
-module(lorawan_prometheus).
-behaviour(gen_server).

-export([start_link/0, event/3, warning/3, error/3, uplink/1, downlink/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(METRICS_URL, "/metrics/[:registry]").

-record(state, {enabled}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

event(warning, {server, Eid}, Text) ->
    warning(server, Eid, Text);
event(warning, {connector, Eid}, Text) ->
    warning(connector, Eid, Text);
event(warning, {Entity, Eid}, Text) ->
    warning(Entity, lorawan_utils:binary_to_hex(Eid), Text);
event(error, {server, Eid}, Text) ->
    error(server, Eid, Text);
event(error, {connector, Eid}, Text) ->
    error(connector, Eid, Text);
event(error, {Entity, Eid}, Text) ->
    error(Entity, lorawan_utils:binary_to_hex(Eid), Text);
event(_Severity, _Source, _Text) ->
    ok.

warning(Entity, Eid, Error) ->
    gen_server:cast(?MODULE, {warning, Entity, Eid, Error}).

error(Entity, Eid, Error) ->
    gen_server:cast(?MODULE, {error, Entity, Eid, Error}).

uplink(MAC) ->
    gen_server:cast(?MODULE, {uplink, lorawan_utils:binary_to_hex(MAC)}).

downlink(MAC) ->
    gen_server:cast(?MODULE, {downlink, lorawan_utils:binary_to_hex(MAC)}).

init([]) ->
    State = case application:get_env(lorawan_server, enable_prometheus, false) of
        false ->
            #state{enabled=false};
        true ->
            create_counters(),
            lorawan_http_registry:update({prometheus, lorawan},
                #{routes => [{?METRICS_URL, prometheus_cowboy2_handler, []}]}),
            #state{enabled=true}
    end,
    {ok, State}.

handle_call(_Request, _From, State) ->
    {stop, {error, unknownmsg}, State}.

handle_cast(_Message, #state{enabled=false}=State) ->
    {noreply, State};
handle_cast({warning, Entity, Eid, Error}, State) ->
    prometheus_counter:inc(lorawan_warnings_total, [Entity, Eid, Error], 1),
    {noreply, State};
handle_cast({error, Entity, Eid, Error}, State) ->
    prometheus_counter:inc(lorawan_errors_total, [Entity, Eid, Error], 1),
    {noreply, State};
handle_cast({uplink, MAC}, State) ->
    prometheus_counter:inc(lorawan_uplinks_total, [MAC], 1),
    {noreply, State};
handle_cast({downlink, MAC}, State) ->
    prometheus_counter:inc(lorawan_downlinks_total, [MAC], 1),
    {noreply, State}.

handle_info(_Message, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

create_counters() ->
    prometheus_counter:declare([{name, lorawan_warnings_total},
                                {help, "Number of warnings registered by the server"},
                                {labels, [entity, eid, warning]}]),
    prometheus_counter:declare([{name, lorawan_errors_total},
                                {help, "Number of errors registered by the server"},
                                {labels, [entity, eid, error]}]),
    prometheus_counter:declare([{name, lorawan_uplinks_total},
                                {help, "Number of uplink frames received by the server"},
                                {labels, [eid]}]),
    prometheus_counter:declare([{name, lorawan_downlinks_total},
                                {help, "Number of downlink frames sent by the server"},
                                {labels, [eid]}]).
