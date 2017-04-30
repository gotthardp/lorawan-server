# Development Guidelines

## Software Architecture

The following figure shows a process hierarchy of the lorawan-server:

![alt tag](https://raw.githubusercontent.com/gotthardp/lorawan-server/master/doc/images/software-architecture.png)

The LoRaWAN communication is handled by the following modules:
 * lorawan_gw_forwarder implements the
   [packet_forwarder protocol](https://github.com/Lora-net/packet_forwarder/blob/master/PROTOCOL.TXT)
 * lorawan_gw_router handles communication to multiple gateways. It performs uplink frame
   deduplication and downlink gateway selection.
 * lorawan_mac performs the LoRaWAN frame encoding and encryption.
 * lorawan_mac_commands implements the LoRaWAN MAC commands, including [ADR](ADR.md)
   algorithms.
 * lorawan_mac_regions contains the region specific functions and constants.
 * lorawan_handler invokes application modules for uplink processing and provides
   functions for downlink scheduling. This is the interface between the LoRaWAN layer
   and the application layer of the server.

The [Cowboy HTTP sever](https://ninenines.eu/docs/en/cowboy/2.0/guide/introduction/)
starts one process for each incoming each connection:
 * lorawan_admin_... modules provide handlers for the REST API. They interact with
   the mnesia database only.
 * lorawan_ws_frames provide handler for the [WebSocket](WebSockets.md) interface.
   The handler joins a [pg2 process group](http://erlang.org/doc/man/pg2.html)
   corresponding to the request URI.


## Debugging

### Logging

Using server logs is the most common technique to debug server functions.
Please review the [lager framework](https://github.com/basho/lager) documentation
for a comprehensive description.

For example, you can generate log messages by doing the following:
```erlang
lager:warning("unexpected value: ~w", [Value])
```

### Connections to the lorawan node

The lorawan-server is started in a cluster node `lorawan@<hostname>`. For example,
if your `hostname` is `debian.home`, the server runs on `lorawan@debian`.

You can call functions on another node by `rpc:call(Node, Module, Function, Args)`.

You can setup a connection to another node by `net_adm:ping(Node)` and then
interact with its processes.

Or you can start the [Observer](http://erlang.org/doc/apps/observer/observer_ug.html)
and just trace the lorawan-server processes and activities.

```bash
erl -smp -sname observer -hidden -setcookie MyCookie -run observer
```

Please note that nodes allowed to communicate with each other use the same
**magic cookie**. Make sure that `$HOME/.erlang.cookie` is the same, or
enter the cookie explicitly by the `-setcookie MyCookie` parameter.
