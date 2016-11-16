# Building Custom Handlers

To implement a new application you need to create a `lorawan_application_xxx.erl`
module implementing the `lorawan_application` behaviour and register it in the
[`sys.config`](lorawan_server.config):
```erlang
{lorawan_server, [
    {plugins, [
        {<<"my-app">>, lorawan_application_xxx},
        ...
    ]}
```

Your module needs to export `init/1`, `handle_join/3` and `handle_rx/5` functions.

The `init/1` will be called upon server initialization. It shall return either
`ok` or a tuple `{ok, PathsList}` to define application specific URIs (e.g.
REST API or WebSockets). For more details see
[Cowboy Routing](https://github.com/ninenines/cowboy/blob/master/doc/src/guide/routing.asciidoc).

The `handle_join/3` will be called when a new node joins the network. The function
shall return either `ok` or `{error, error_description}`.

The `handle_rx/5` will be called upon reception of a LoRaWAN frame. The function
shall return `ok`, `{error, error_description}` or a tuple `{send, PortOut, DataOut}`
to send a response back.

```erlang
handle_rx(DevAddr, <<"my-app">>, AppID, PortIn, <<"DataIn">>) ->
    %% application logic
    %% ...
    {send, PortOut, <<"DataOut">>}.
```
