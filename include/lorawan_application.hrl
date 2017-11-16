%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
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
    tmst :: integer(),
    srvtmst :: integer(), % when received by the server
    rssi :: number(),
    lsnr :: number()}).

-record(txq, {
    region :: binary(),
    freq :: number(),
    datr :: binary() | integer(),
    codr :: binary(),
    tmst :: 'undefined' | integer(),
    time :: 'undefined' | 'immediately' | calendar:datetime(),
    powe :: 'undefined' | integer()}).

-record(user, {
    name :: nonempty_string(),
    pass :: string(),
    roles :: [string()]}).

-record(network, {
    name :: nonempty_string(),
    netid :: binary(), % network id
    subid :: 'undefined' | bitstring(), % sub-network id
    region :: binary(),
    max_eirp :: 'undefined' | integer()}).

-record(gateway, {
    mac :: binary(),
    network :: nonempty_string(),
    tx_rfch :: integer(), % rf chain for downlinks
    tx_powe :: 'undefined' | integer(),
    ant_gain :: 'undefined' | integer(),
    group :: any(),
    desc :: 'undefined' | string(),
    gpspos :: {number(), number()}, % {latitude, longitude}
    gpsalt :: 'undefined' | number(), % altitude
    last_rx :: 'undefined' | calendar:datetime(),
    dwell :: [{calendar:datetime(), {number(), number(), number()}}], % {frequency, duration, hoursum}
    delays :: [{calendar:datetime(), integer()}]}).

-record(multicast_channel, {
    devaddr :: devaddr(), % multicast address
    network :: nonempty_string(),
    app :: binary(),
    appid :: any(), % application route
    nwkskey :: seckey(),
    appskey :: seckey(),
    mac :: binary(),
    fcntdown :: integer()}). % last downlink fcnt

-record(profile, {
    name :: nonempty_string(),
    region :: binary(),
    app :: binary(),
    appid :: any(), % application route
    fcnt_check :: integer(),
    txwin :: integer(),
    adr_flag_set :: 0..4, % server requests
    adr_set :: adr_config(), % requested after join
    rxwin_set :: rxwin_config(), % requested
    request_devstat :: boolean()}).

-record(device, {
    deveui :: eui(),
    profile :: nonempty_string(),
    appargs :: any(), % application arguments
    appeui :: eui(),
    appkey :: seckey(),
    node :: devaddr(),
    can_join :: boolean(),
    last_join :: calendar:datetime()}).

-record(node, {
    devaddr :: devaddr(),
    profile :: nonempty_string(),
    appargs :: any(), % application arguments
    fcntup :: integer(), % last uplink fcnt
    fcntdown :: integer(), % last downlink fcnt
    first_reset :: calendar:datetime(),
    last_reset :: calendar:datetime(),
    reset_count :: integer(), % number of resets/joins
    last_rx :: 'undefined' | calendar:datetime(),
    last_mac :: binary(), % gateway used
    last_rxq :: #rxq{},
    adr_flag_use :: 0..1, % device supports
    adr_use :: adr_config(), % used
    rxwin_use :: rxwin_config(), % used
    last_qs :: [{integer(), integer()}], % list of {RSSI, SNR} tuples
    devstat_time :: 'undefined' | calendar:datetime(),
    devstat_fcnt :: 'undefined' | integer(),
    devstat :: [{calendar:datetime(), integer(), integer()}]}). % {time, battery, margin}

-record(rxdata, {
    fcnt :: integer(),
    port :: integer(),
    data :: binary(),
    last_lost=false :: boolean(),
    shall_reply=false :: boolean()}).

-record(txdata, {
    confirmed=false :: boolean(),
    port :: 'undefined' | integer(),
    data :: 'undefined' | binary(),
    pending :: 'undefined' | boolean()}).

-record(pending, {
    devaddr :: devaddr(),
    confirmed :: boolean(),
    phypayload :: binary()}).

-record(txframe, {
    frid :: frid(), % unique identifier
    datetime :: calendar:datetime(),
    devaddr :: devaddr(),
    txdata :: #txdata{}}).

-record(rxframe, {
    frid :: frid(), % unique identifier
    mac :: binary(), % gateway used
    rxq :: #rxq{},
    average_qs :: 'undefined' | {number(), number()}, % average RSSI and SNR
    app :: binary(),
    appid :: any(), % application route
    region :: binary(),
    devaddr :: devaddr(),
    powe:: integer(),
    fcnt :: integer(),
    confirm :: boolean(),
    port :: integer(),
    data :: binary(),
    datetime :: calendar:datetime()}).

-record(event, {
    evid :: binary(),
    severity :: atom(),
    first_rx :: calendar:datetime(),
    last_rx :: calendar:datetime(),
    count :: integer(),
    entity :: atom(),
    eid :: binary(),
    text :: binary(),
    args :: 'undefined' | binary()}).

% end of file
