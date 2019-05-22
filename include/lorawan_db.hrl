%
% Copyright (c) 2016-2019 Petr Gotthard <petr.gotthard@centrum.cz>
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
    reserved :: any(), %% for future use
    rssi :: number(),
    lsnr :: number()}).

-record(txq, {
    freq :: number(),
    datr :: binary() | integer(),
    codr :: binary(),
    time :: integer() | 'immediately' | calendar:datetime(),
    powe :: 'undefined' | integer()}).

-record(area, {
    name :: nonempty_string(),
    region :: binary(),
    admins :: [nonempty_string()],
    slack_channel :: 'undefined' | string(),
    log_ignored :: boolean()}).

-record(gateway, {
    mac :: binary(),
    area :: 'undefined' | nonempty_string(),
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
    delays :: [{calendar:datetime(), {integer(), integer(), integer()}}], % {min, avg, max}
    health_alerts :: [atom()],
    health_decay :: integer(),
    health_reported :: integer(),
    health_next :: 'undefined' | calendar:datetime()}).

-record(multicast_channel, {
    devaddr :: devaddr(), % multicast address
    profiles :: [nonempty_string()],
    nwkskey :: seckey(),
    appskey :: seckey(),
    fcntdown :: integer()}). % last downlink fcnt

-record(network, {
    name :: nonempty_string(),
    netid :: binary(), % network id
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
    dcycle_init :: integer(),
    rxwin_init :: rxwin_config(),
    init_chans :: intervals(),
    cflist :: 'undefined' | [{number(), integer(), integer()}]}).

-record(group, {
    name :: nonempty_string(),
    network :: nonempty_string(),
    subid :: 'undefined' | bitstring(), % sub-network id
    admins :: [nonempty_string()],
    slack_channel :: 'undefined' | string(),
    can_join :: boolean()}).

-record(profile, {
    name :: nonempty_string(),
    group :: nonempty_string(),
    app :: binary(),
    appid :: any(),
    join :: 0..2,
    fcnt_check :: integer(),
    txwin :: integer(),
    adr_mode :: 0..2, % server requests
    adr_set :: adr_config(), % requested after join
    max_datr :: 'undefined' | number(),
    dcycle_set :: integer(),
    rxwin_set :: rxwin_config(), % requested
    request_devstat :: boolean()}).

-record(device, {
    deveui :: eui(),
    profile :: nonempty_string(),
    appargs :: any(), % application arguments
    appeui :: eui(),
    appkey :: seckey(),
    desc :: 'undefined' | string(),
    last_joins :: [{calendar:datetime(), binary()}],
    node :: devaddr()}).

-type devstat() :: {calendar:datetime(), integer(), integer(), integer()}.

-record(node, {
    devaddr :: devaddr(),
    profile :: nonempty_string(),
    appargs :: any(), % application arguments
    nwkskey :: seckey(),
    appskey :: seckey(),
    desc :: 'undefined' | string(),
    location :: 'undefined' | string(),
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
    dcycle_use :: integer(),
    rxwin_use :: rxwin_config(), % used
    rxwin_failed=[] :: [binary()], % last request failed
    last_qs :: [{integer(), integer()}], % list of {RSSI, SNR} tuples
    average_qs :: 'undefined' | {number(), number()}, % average RSSI and SNR
    devstat_time :: 'undefined' | calendar:datetime(),
    devstat_fcnt :: 'undefined' | integer(),
    devstat :: [devstat()], % {time, battery, margin, max_snr}
    health_alerts :: [atom()],
    health_decay :: integer(),
    health_reported :: integer(),
    health_next :: 'undefined' | calendar:datetime()}).

-record(ignored_node, {
    devaddr :: devaddr(),
    mask :: devaddr()}).

-record(connector, {
    connid :: binary(),
    app :: binary(),
    format :: binary(),
    uri :: binary(),
    publish_qos :: 0 | 1 | 2,
    publish_uplinks :: 'undefined' | binary(),
    publish_events :: 'undefined' | binary(),
    subscribe_qos :: 0 | 1 | 2,
    subscribe :: 'undefined' | binary(),
    received :: 'undefined' | binary(),
    enabled :: boolean(),
    failed=[] :: [binary()],
    client_id :: 'undefined' | binary(),
    auth :: binary(),
    name :: 'undefined' | binary(),
    pass :: 'undefined' | binary(),
    certfile :: 'undefined' | binary(),
    keyfile :: 'undefined' | binary(),
    health_alerts :: [atom()],
    health_decay :: integer(),
    health_reported :: integer(),
    health_next :: 'undefined' | calendar:datetime()}).

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

-record(queued, {
    frid :: frid(), % unique identifier
    datetime :: calendar:datetime(),
    devaddr :: devaddr(),
    txdata :: #txdata{}}).

-record(pending, {
    devaddr :: devaddr(),
    confirmed :: boolean(),
    phypayload :: binary(),
    sent_count :: integer(),
    receipt :: any()}).

-record(rxframe, {
    frid :: frid(), % unique identifier
    dir :: binary(),
    network :: nonempty_string(),
    app :: binary(),
    devaddr :: devaddr(),
    location :: any(),
    gateways :: [{binary(), #rxq{}}], % singnal quality at each gateway
    average_qs :: 'undefined' | {number(), number()}, % average RSSI and SNR
    powe:: integer(),
    fcnt :: integer(),
    confirm :: boolean(),
    port :: integer(),
    data :: binary(),
    datetime :: calendar:datetime()}).

% end of file
