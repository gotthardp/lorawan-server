%
% Copyright (c) 2016-2018 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%

-type eui() :: <<_:64>>.
-type seckey() :: <<_:128>>.
-type devaddr() :: <<_:32>>.
-type frid() :: <<_:64>>.
-type intervals() :: [{integer(), integer()}].
-type adr_config() :: {integer(), integer(), intervals()}.
-type rxwin_config() :: {
    'undefined' | integer(),
    'undefined' | integer(),
    'undefined' | number()}.

-record(rxq, {
    freq :: number(),
    datr :: binary() | integer(),
    codr :: binary(),
    time :: calendar:datetime(),
    tmms :: integer(),
    tmst :: integer(),
    rssi :: number(),
    lsnr :: number()}).

-record(txq, {
    freq :: number(),
    datr :: binary() | integer(),
    codr :: binary(),
    tmst :: 'undefined' | integer(),
    time :: 'undefined' | 'immediately' | calendar:datetime(),
    powe :: 'undefined' | integer()}).

-record(network, {
    name :: nonempty_string(),
    netid :: binary(), % network id
    subid :: 'undefined' | bitstring(), % sub-network id
    region :: binary(),
    tx_codr :: binary(),
    join1_delay :: integer(),
    join2_delay :: integer(),
    rx1_delay :: integer(),
    rx2_delay :: integer(),
    gw_power :: integer(),
    max_eirp :: integer(),
    max_power :: integer(),
    min_power :: integer(),
    max_datr :: number(),
    rxwin_init :: rxwin_config(),
    init_chans :: intervals(),
    cflist :: 'undefined' | [{number(), integer(), integer()}]}).

-record(gateway, {
    mac :: binary(),
    group :: any(),
    tx_rfch :: integer(), % rf chain for downlinks
    ant_gain :: integer(), % antenna gain
    desc :: 'undefined' | string(),
    gpspos :: {number(), number()}, % {latitude, longitude}
    gpsalt :: 'undefined' | number(), % altitude
    ip_address :: {inet:ip_address(), inet:port_number(), integer()},
    last_alive :: 'undefined' | calendar:datetime(),
    last_gps :: 'undefined' | calendar:datetime(),
    last_report :: 'undefined' | calendar:datetime(),
    dwell :: [{calendar:datetime(), {number(), number(), number()}}], % {frequency, duration, hoursum}
    delays :: [{calendar:datetime(), {integer(), integer(), integer()}}]}). % {min, avg, max}

-record(multicast_channel, {
    devaddr :: devaddr(), % multicast address
    profiles :: [nonempty_string()],
    nwkskey :: seckey(),
    appskey :: seckey(),
    fcntdown :: integer()}). % last downlink fcnt

-record(profile, {
    name :: nonempty_string(),
    network :: nonempty_string(),
    app :: binary(),
    appid :: any(),
    can_join :: boolean(),
    fcnt_check :: integer(),
    txwin :: integer(),
    adr_mode :: 0..2, % server requests
    adr_set :: adr_config(), % requested after join
    max_datr :: 'undefined' | number(),
    rxwin_set :: rxwin_config(), % requested
    request_devstat :: boolean()}).

-record(device, {
    deveui :: eui(),
    profile :: nonempty_string(),
    appargs :: any(), % application arguments
    appeui :: eui(),
    appkey :: seckey(),
    desc :: 'undefined' | string(),
    last_join :: calendar:datetime(),
    node :: devaddr()}).

-type devstat() :: {calendar:datetime(), integer(), integer(), integer()}.

-record(node, {
    devaddr :: devaddr(),
    profile :: nonempty_string(),
    appargs :: any(), % application arguments
    nwkskey :: seckey(),
    appskey :: seckey(),
    desc :: 'undefined' | string(),
    fcntup :: 'undefined' | integer(), % last uplink fcnt
    fcntdown :: integer(), % last downlink fcnt
    first_reset :: calendar:datetime(),
    last_reset :: calendar:datetime(),
    reset_count :: integer(), % number of resets/joins
    last_rx :: 'undefined' | calendar:datetime(),
    gateways :: [{binary(), #rxq{}}], % last seen gateways
    adr_flag :: 0..1, % device supports
    adr_set :: 'undefined' | adr_config(), % auto-calculated
    adr_use :: adr_config(), % used
    adr_failed=[] :: [binary()], % last request failed
    rxwin_use :: rxwin_config(), % used
    rxwin_failed=[] :: [binary()], % last request failed
    last_qs :: [{integer(), integer()}], % list of {RSSI, SNR} tuples
    average_qs :: 'undefined' | {number(), number()}, % average RSSI and SNR
    devstat_time :: 'undefined' | calendar:datetime(),
    devstat_fcnt :: 'undefined' | integer(),
    devstat :: [devstat()]}). % {time, battery, margin, max_snr}

-record(ignored_node, {
    devaddr :: devaddr(),
    mask :: devaddr()}).

-record(connector, {
    connid :: binary(),
    app :: binary(),
    format :: binary(),
    uri :: binary(),
    publish_uplinks :: 'undefined' | binary(),
    publish_events :: 'undefined' | binary(),
    subscribe :: 'undefined' | binary(),
    received :: 'undefined' | binary(),
    enabled :: boolean(),
    failed=[] :: [binary()],
    client_id :: 'undefined' | binary(),
    auth :: binary(),
    name :: 'undefined' | binary(),
    pass :: 'undefined' | binary(),
    certfile :: 'undefined' | binary(),
    keyfile :: 'undefined' | binary()}).

-define(EMPTY_PATTERN, {<<>>,[]}).

-record(handler, {
    app :: binary(),
    uplink_fields :: [binary()],
    payload :: 'undefined' | binary(),
    parse_uplink :: 'undefined' | {binary(), fun()},
    event_fields :: [binary()],
    parse_event :: 'undefined' | {binary, fun()},
    build :: 'undefined' | {binary(), fun()},
    downlink_expires :: binary()}).

-record(txdata, {
    confirmed=false :: boolean(),
    port :: 'undefined' | integer(),
    data :: 'undefined' | binary(),
    pending :: 'undefined' | boolean(),
    receipt :: any()}).

-record(pending, {
    devaddr :: devaddr(),
    confirmed :: boolean(),
    phypayload :: binary(),
    receipt :: any()}).

-record(txframe, {
    frid :: frid(), % unique identifier
    datetime :: calendar:datetime(),
    devaddr :: devaddr(),
    txdata :: #txdata{}}).

-record(rxframe, {
    frid :: frid(), % unique identifier
    network :: nonempty_string(),
    app :: binary(),
    devaddr :: devaddr(),
    gateways :: [{binary(), #rxq{}}], % singnal quality at each gateway
    average_qs :: 'undefined' | {number(), number()}, % average RSSI and SNR
    powe:: integer(),
    fcnt :: integer(),
    confirm :: boolean(),
    port :: integer(),
    data :: binary(),
    datetime :: calendar:datetime()}).

% end of file
