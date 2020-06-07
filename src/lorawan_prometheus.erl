%
%
-module(lorawan_prometheus).
-behaviour(gen_server).

%% Public API
-export([start_link/0, event/3, warning/3, error/3, uplink/1, downlink/1]).

%% gen_server Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% Settings
-define(METRICS_URL, "/metrics/[:registry]").
-record(state, {enabled=false}).

%% Public API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% Deal with value tuples first, ack_lost will match the second clause
event(Severity, Source, {Text, Num}) when is_integer(Num) ->
    event0(Severity, Source, Text, Num);
event(Severity, Source, {Text, _Val}) ->
    event0(Severity, Source, Text, 1);
event(Severity, Source, Text) ->
    event0(Severity, Source, Text, 1).

warning(Source, Text, Num) ->
    gen_server:cast(?MODULE, {warning, Source, Text, Num}).

error(Source, Text, Num) ->
    gen_server:cast(?MODULE, {error, Source, Text, Num}).

uplink(MAC) ->
    gen_server:cast(?MODULE, {uplink, lorawan_utils:binary_to_hex(MAC)}).

downlink(MAC) ->
    gen_server:cast(?MODULE, {downlink, lorawan_utils:binary_to_hex(MAC)}).

%% gen_server Callbacks
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
handle_cast({warning, Source, Text, Num}, State) ->
    {Entity, Eid} = expand_source(Source),
    prometheus_counter:inc(lorawan_warnings_total, [Entity, Eid, Text], Num),
    {noreply, State};
handle_cast({error, Source, Text, Num}, State) ->
    {Entity, Eid} = expand_source(Source),
    prometheus_counter:inc(lorawan_errors_total, [Entity, Eid, Text], Num),
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

%% Private functions

% Register Prometheus metrics
create_counters() ->
    prometheus_counter:declare([{name, lorawan_warnings_total},
                                {help, "Number of warnings registered by the server"},
                                {labels, [entity, eid, warning]}]),
    prometheus_counter:declare([{name, lorawan_errors_total},
                                {help, "Number of errors registered by the server"},
                                {labels, [entity, eid, error]}]),
    prometheus_counter:declare([{name, lorawan_uplinks_total},
                                {help, "Number of uplink frames received by the server"},
                                {labels, [gateway]}]),
    prometheus_counter:declare([{name, lorawan_downlinks_total},
                                {help, "Number of downlink frames sent by the server"},
                                {labels, [gateway]}]).

% Filter errors and warnings
event0(warning, Source, Text, Num) ->
    warning(Source, Text, Num);
event0(error, Source, Text, Num) ->
    error(Source, Text, Num);
event0(_Severity, _Source, _Text, _Num) ->
    ok.

% Expand event source, converting Id to HEX where necessary
expand_source({server, Eid})    -> {server, Eid};
expand_source({connector, Eid}) -> {connector, Eid};
expand_source({handler, Eid})   -> {handler, Eid};
% node, device, gateway, multicast_channel
expand_source({Other, Eid})     -> {Other, lorawan_utils:binary_to_hex(Eid)};
% uncaught in lorawan_utils:throw_event/3-4
expand_source(Source)           -> {Source, undefined}.
