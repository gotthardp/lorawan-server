# Building Custom Applications

By default you will see 4 different applications. Two wrappers for connecting to
external applications:
 * *backend* for connecting to [MQTT Backends](Backends.md).
 * *websocket* for receiving connections from [WebSocket clients](WebSockets.md).

And two very simple internal applications for existing motes:
 * *semtech-mote* for
   [Semtech/IMST LoRaMote](http://webshop.imst.de/loramote-lora-evaluation-tool.html)
 * *microchip-mote* for
   [Microchip LoRa(TM) Technology Mote](http://www.microchip.com/Developmenttools/ProductDetails.aspx?PartNO=dm164138)

You may follow these examples and create your own internal applications.

Each application may implement:
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
    {plugins, [
        {<<"my-app">>, lorawan_application_xxx},
        ...
    ]}
```

The custom application may be implemented as a standalone Erlang application `app_x`.
In such case do:
```erlang
{lorawan_server, [
    {plugins, [
        {<<"my-app">>, {app_x, lorawan_application_xxx}},
        ...
    ]}
```

This standalone Erlang application `app_x` may be included in the lorawan-server
release defined in the `rebar.config`:
```erlang
{deps, [
    {app_x, {git, "https://github.com/your_account/app-x.git", {branch, "master"}}},
    ...
]}.
{relx, [
    {release, {'lorawan-server', "0.0.0"},
        [lorawan_server, app_x]},
    ...
]}.
```

## lorawan_application Behaviour

Your module needs to export `init/1`, `handle_join/3` and `handle_rx/5` functions.

### init(App)

The `init/1` will be called upon server initialization. It shall return either
`ok` or a tuple `{ok, PathsList}` to define application specific URIs (e.g.
REST API or WebSockets). For more details see
[Cowboy Routing](https://github.com/ninenines/cowboy/blob/master/doc/src/guide/routing.asciidoc).

### handle_join(DevAddr, AppID, AppArgs)

The `handle_join/3` will be called when a new node joins the network. The function
shall return either `ok` or `{error, error_description}`.

### handle_rx(DevAddr, AppID, AppArgs, RxData, RxQ)

The `handle_rx/5` will be called upon reception of a LoRaWAN frame:
  * *DevAddr* is the 4-byte device address
  * *AppID* is an application-specific device group or behaviour
  * *AppArgs* is an opaque string with application-specific settings
  * *RxData* is the #rxdata{} record with:
    * *fcnt*
    * *port* number
    * *data* binary
    * *last_lost* flag indicating that the last confirmed response got lost
    * *shall_reply* flag indicating the MAC has to reply to the device even if
      the application sends no data
  * *RxQ* contains the #rxq{} record with frame reception details

The *last_lost* flag allows the application to decide to send new data instead of
retransmitting the old data.

The *shall_reply* flag allows the application to send data when a downlink frame
needs to be transmitted anyway due to a MAC layer decision.
This flag is set when:
  * Confirmed uplink was received, which needs to be acknowledged
  * ADR ACK was requested by the device
  * MAC layer needs to send a command back to the device

The function may return:
  * *ok* to not send any response
  * *retransmit* to re-send the last frame (when *#rxdata.last_lost* was *true*)
  * *{send, #txdata{}}* to send a response back, where the #txdata{} record may include:
    * *port* number
    * *data* binary
    * *confirmed* flag to indicate a confirmed response (default is *false*)
    * *pending* flag to indicates the application has more data to send (default is *false*)
  * *{error, error_description}* to record a failure and send nothing

For example:
```erlang
handle_rx(DevAddr, <<"my-app">>, AppArgs, #rxdata{last_lost=true}) ->
    retransmit;
handle_rx(DevAddr, <<"my-app">>, AppArgs, #rxdata{port=PortIn, data= <<"DataIn">>}) ->
    %% application logic
    %% ...
    {send, #txdata{port=PortOut, data= <<"DataOut">>}}.
```

## HTTP Server

lorawan-server integrates the Cowboy HTTP server. The applications may use the
full potential of this server.
Please refer to the [Cowboy 2.0 User Guide](https://ninenines.eu/docs/en/cowboy/2.0/guide/),
or [Cowboy 2.0 Function Reference](https://ninenines.eu/docs/en/cowboy/2.0/manual/)
for more details.
