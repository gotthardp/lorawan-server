# Building Custom Handlers

To implement a new application you need to create a new `lorawan_application_xxx.erl` module
and register it in the [`sys.config`](lorawan_server.config):
```erlang
{lorawan_server, [
    {plugins, [
        {<<"my-app">>, lorawan_application_xxx},
        ...
    ]}
```

Your module needs to export `init/1` and the `handle/5` function for data processing.

The `init/1` shall return either `ok` or a tuple `{ok, PathsList}` to define
application specific URIs (e.g. REST API or WebSockets).
For more details see [Cowboy Routing](https://github.com/ninenines/cowboy/blob/master/doc/src/guide/routing.asciidoc).

The `handle/5` function shall return either `ok` or a tuple `{send, PortOut, DataOut}`
to send a response back.

```erlang
handle(DevAddr, <<"my-app">>, AppID, PortIn, <<"DataIn">>) ->
    %% application logic
    %% ...
    {send, PortOut, <<"DataOut">>}.
```
