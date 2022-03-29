# Building Custom Applications

The server applications can be:
 * External applications defined by the [Handlers](Handlers.md)
 * Internal applications implemented in Erlang

After a fresh installation you will see only one internal application:
 - **semtech-mote** for
   [Semtech/IMST LoRaMote](http://webshop.imst.de/loramote-lora-evaluation-tool.html)

Sample lorawan-server extension, which implements a **microchip-mote** application
for [Microchip LoRa(TM) Technology Mote](http://www.microchip.com/Developmenttools/ProductDetails.aspx?PartNO=dm164138),
is available at
https://github.com/gotthardp/lorawan-server-demoapp.

You may fork this example and create your own internal applications.

Each internal application may implement:
  * LoRaWAN application handlers (see `lorawan_application` behaviour);
  * HTTP server handlers for
[static files](https://ninenines.eu/docs/en/cowboy/2.0/guide/static_files/),
[REST](https://ninenines.eu/docs/en/cowboy/2.0/guide/rest_handlers/) or
[Websockets](https://ninenines.eu/docs/en/cowboy/2.0/guide/ws_handlers/).

To implement a new application you need to create a `lorawan_application_xxx.erl`
module implementing the `lorawan_application` behaviour and register it in the
[`sys.config`](../lorawan_server.config):
```erlang
{lorawan_server, [
    {applications, [
        {<<"my-app">>, lorawan_application_xxx},
        ...
    ]}
```

The custom application may be implemented as a standalone Erlang application `app_x`.
In such case do:
```erlang
{lorawan_server, [
    {applications, [
        {<<"my-app">>, {app_x, lorawan_application_xxx}},
        ...
    ]}
```

You may write a new `rebar.config` and create a rebar release bundling together
your custom `app_x` and the standard lorawan-server release.
```erlang
{erl_opts, [
    {parse_transform, lager_transform}
]}.
{deps, [
    {lorawan_server, {git, "https://github.com/gotthardp/lorawan-server.git", {branch, "master"}}}
]}.
{relx, [
    {release, {'lorawan-server-appx', "0.1.0"},
        [lorawan_server, lorawan_appx]},
    {sys_config, "lorawan_demoapp.config"}
]}.
```

## lorawan_application Behaviour

Your module needs to export `init/1`, `handle_join/3`, `handle_uplink/4`,
`handle_rxq/5` and `handle_delivery/3` functions.

### init(App)

The `init/1` will be called upon server initialization. It shall return either
`ok` or a tuple `{ok, PathsList}` to define application specific URIs (e.g.
REST API or WebSockets). For more details see
[Cowboy Routing](https://github.com/ninenines/cowboy/blob/master/doc/src/guide/routing.asciidoc).

### handle_join({Network, Profile, Device}, {MAC, RxQ}, DevAddr)

The `handle_join/3` will be called when a new node joins the network. The function
shall return either `ok` or `{error, error_description}`.

### handle_uplink({Network, Profile, Node}, {MAC, RxQ}, LastMissed, Frame)

The `handle_uplink/4` will be called upon first reception of a LoRaWAN frame
(before deduplication):
  - **Network** parameters where the node is operating
  - **Profile** of the node
  - **Node** configuration
  - **MAC** of the gateway that first received this frame (this doesn't have to be
    the best gateway)
  - **RxQ** with reception quality at the first gateway
  - **LastMissed** can be
    - {missed, Receipt} when the last downlink was confirmed and got lost
    - `undefined` otherwise
  - **Frame** is the #frame{} record with:
    - **fcnt**
    - **port** number
    - **data** binary
  - **RxQ** contains the #rxq{} record with frame reception details

The *last_lost* flag allows the application to decide to send new data instead of
retransmitting the old data.

The function may return:
  - **{ok, State}** to continue processing in `handle_rxq/5`
  - **retransmit** to re-send the last frame (when #rxdata.last_lost was `true`)
  - **{error, error_description}** to record a failure and send nothing

### handle_rxq({Network, Profile, Node}, Gateways, WillReply, Frame, State)

The `handle_rxq/5` will be called after receiving the frame from all gateways
(after deduplication):
  - **Network** parameters where the node is operating
  - **Profile** of the node
  - **Node** configuration
  - **Gateways** that received the frame, sorted based on RSSI (the best first),
    which is a list of tuples {MAC, RxQ}, where:
    - **MAC** of the gateway that received the frame
    - **RxQ** with reception quality at this gateway
  - **WillReply** flag indicating the MAC is about to reply to the device even if
    the application sends no data
  - **Frame** is the #frame{} record
  - **State** received from `handle_uplink/4`

The **WillReply** flag allows the application to send data when a downlink frame
needs to be transmitted anyway due to a MAC layer decision.
This flag is set when:
  * Confirmed uplink was received, which needs to be acknowledged
  * ADR ACK was requested by the device
  * MAC layer needs to send a command back to the device

The function may return:
  - **ok** to not send any response
  - **{send, #txdata{}}** to send a response back, where the #txdata{} record may include:
    - **port** number
    - **data** binary
    - **confirmed** flag to indicate a confirmed response (default is `false`)
    - **pending** flag to indicates the application has more data to send (default is `false`)
    - **receipt** field to include any opaque data
  - **{error, error_description}** to record a failure and send nothing

### handle_delivery({Network, Profile, Node}, Result, Receipt)

The `handle_delivery/3` will be called after successful or unsuccessful delivery
of a confirmed downlink frame:
  - **Network** parameters where the node is operating
  - **Profile** of the node
  - **Node** configuration
  - **Result** can be
    - **delivered**
    - **lost**
  - **Receipt** is the value included in the downlink #txdata

## HTTP Server

lorawan-server integrates the Cowboy HTTP server. The applications may use the
full potential of this server.
Please refer to the [Cowboy 2.0 User Guide](https://ninenines.eu/docs/en/cowboy/2.0/guide/),
or [Cowboy 2.0 Function Reference](https://ninenines.eu/docs/en/cowboy/2.0/manual/)
for more details.
