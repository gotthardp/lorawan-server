# Development Guidelines

## Software Architecture

The following figure shows a process hierarchy of the lorawan-server:

![alt tag](https://raw.githubusercontent.com/gotthardp/lorawan-server/master/doc/images/software-architecture.png)

The LoRaWAN communication is handled by the following modules:
 - **lorawan_gw_forwarder** implements the
   [packet_forwarder protocol](https://github.com/Lora-net/packet_forwarder/blob/master/PROTOCOL.TXT)
 - **lorawan_gw_router** handles communication to multiple gateways. It performs uplink frame
   deduplication and downlink gateway selection.
 - **lorawan_handler** is started for each uplink LoRaWAN frame
   - invokes **lorawan_mac** to decode and decrypt the performs the frame
   - invokes **lorawan_mac_commands** to handle the MAC commands, including ADR
     algorithms
   - invokes the desired internal application or **lorawan_application_backend**
 - **lorawan_backend_factory** starts and stops the individual backend connectors
 - **lorawan_application_backend** extracts desired data fields from the uplink frame
   and forwards the fields to a [pg2 process group](http://erlang.org/doc/man/pg2.html)
   corresponding to the handler name
 - **lorawan_connectors** open and maintain connections to backend systems and
   transmits the data fields to/from them using a desired protocol. Each
   connector joins a given pg2 group

The [Cowboy HTTP sever](https://ninenines.eu/docs/en/cowboy/2.0/guide/introduction/)
starts one process for each incoming each connection:
 - **lorawan_admin_...** modules provide handlers for the REST API. They interact with
   the mnesia database only.
 - **lorawan_connector_ws** provide handler for the WebSocket interface. One
   handler invoked for each incoming connection, to joins a given pg2 group as
   every connector and then filters the uplink frames based on given URI.


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

The lorawan-server is started as a cluster node `lorawan@<hostname>`, where
`<hostname>` is a short hostname of the machine hosting the server. For example,
if `hostname --short` returns `debian`, the server runs as a node `lorawan@debian`.

You can connect to a running lorawan-server by `erl -sname test -remsh lorawan@<hostname>`.
Once connected you can e.g. use the [Mnesia functions](http://erlang.org/doc/man/mnesia.html)
to directly access the server database.

Advanced users can also start the [Observer](http://erlang.org/doc/apps/observer/observer_ug.html)
and trace the lorawan-server processes and activities:

```bash
erl -smp -sname observer -hidden -setcookie MyCookie -run observer
```

Please note that nodes allowed to communicate with each other use the same
**magic cookie**. Make sure that `$HOME/.erlang.cookie` is the same, or
enter the cookie explicitly by the `-setcookie MyCookie` parameter.


## Release Process

To create a new release:

* update the version number in `rebar.config`, `src/lorawan_server.app.src` and `scripts/buildroot/lorawan-server.mk`
* commit and push the change
* create a new signed tag, e.g.

```bash
git tag v0.6.5 master
git push origin v0.6.5
```
