# Server Configuration

This document describes how to configure the lorawan-server.

## Static parameters (sys.config and lorawan_server.config)

Static server configuration is defined in
`lorawan-server/releases/<VERSION>/sys.config`, which is usually stored under
`/usr/lib`. It is a single file has multiple sub-sections like `lorawan_server`,
`lager` or `os_mon.

The `sys.config` is created automatically during the server build (`make release`)
based on the [lorawan_server.config](/lorawan_server.config). The default values
are defined there.

For example:
```erlang
[{lorawan_server, [
    % update this list to add/remove applications
    {applications, [
        {<<"semtech-mote">>, lorawan_application_semtech_mote}]},
    % UDP port listening for packets from the packet_forwarder Gateway
    {packet_forwarder_listen, [{port, 1680}]},
    % HTTP port for web-administration and REST API
    {http_admin_listen, [{port, 8080}]},
    % default username and password for the admin interface
    {http_admin_credentials, {<<"admin">>, <<"admin">>}},
    % amount of rxframes retained for each device/node
    {retained_rxframes, 50},
    % websocket expiration if client sends no data
    {websocket_timeout, 3600000} % ms
]},
{os_mon, [
    % Setting this parameter to true can be necessary on embedded systems with
    % stripped-down versions of Unix tools like df.
    {disksup_posix_only, false}
]}].
```

Review the `sys.config` and modify where needed. After updating the configuration
you need to restart the server.

### lorawan_server section

To enable/disable applications, modify the `applications` section. For more
details see the [Custom Application Guide](Applications.md).

To disable the plain HTTP web-admin, set `{http_admin_listen, []}`.

### lager section

The [lager](https://github.com/erlang-lager/lager#internal-log-rotation) system
is used to create and rotate logs. By default two logs will be created: debug and
error. Lager will rotate each log file at midnight or when it reaches 10MB,
whichever comes first, and keep 5 rotated logs in addition to the current one.

To reduce the amount of storage utilized by the logs and have only 3 files <5MB
modify the lager handlers configuration in your sys.config:
```erlang
{lager_file_backend, [{file, "debug.log"}, {level, debug}, {size, 5242880}, {count, 3}]}
```

### os_mon section

The os_mon is used to monitor the CPU, disk and memory of the underlying
operating system.

Set `{disksup_posix_only, true}` when your embedded system uses stripped-down
Unix tools


## Run-time parameters

Run-time configurtaion can be modified via the web [Administration](Administration.md).

### Infrastructure

You need to:
 - Define at least one administrative *Area*, which is covered by your server.
 - Define at least one LoRaWAN *Gateway* you want to use and assign it to an Area.
 - Define LoRaWAN *Network* parameters according to your geographical region.
   Recommended values are listed in the [List of Regions](Regions.md).

See (Infrastructure Administration)[Infrastructure.md] for more details.

### Devices

You need to:
 - Define at least one administrative *Group*.
 - For each type of a device define a device *Profile* and assign it to a Group.
 - Configure each device you want to use and assign them to one of the *Profiles*:
   - To add a device activated by personalization (ABP), create a new *Nodes* list entry.
   - To add an OTAA device, create a new *Devices* list entry and start the device.
     The *Nodes* list will be updated automatically once the device joins the network.

See (Devices Administration)[Devices.md] for more details.

### Backends

To use an external [application](Applications.md) you need to:
 - For each application define a *Handler*.
 - For each external connection define a *Connector* and link it with the Handler.

See [Handlers](Handlers.md) and [Connectors Administration](Connectors.md) for more details.


## packet_forwarder Setup

The LoRaWAN gateway running the
[`packet_forwarder`](https://github.com/Lora-net/packet_forwarder) needs to forward
received frames to the lorawan_server.

Edit the [`global_conf.json`](https://github.com/Lora-net/packet_forwarder/blob/master/lora_pkt_fwd/global_conf.json)
in your Gateway and update the `server_address`, `serv_port_up` and `serv_port_down` as necessary.

For example:
```json
{
    "gateway_conf": {
        "gateway_ID": "AA555A0000000000",
        "server_address": "server.example.com",
        "serv_port_up": 1680,
        "serv_port_down": 1680,
        "keepalive_interval": 10,
        "stat_interval": 30,
        "push_timeout_ms": 100,
        "forward_crc_valid": true,
        "forward_crc_error": false,
        "forward_crc_disabled": false
    }
}
```

When both packet_forwarder and lorawan-server are running on the same machine
use `localhost` or `127.0.0.1` as the `server_address`.


## Firewall configuration

You may need to enable communication channels from LoRaWAN gateways in your firewall.
If you use the `firewalld` (Fedora, RHEL, CentOS) do:
```bash
cp lorawan-forwarder.xml /usr/lib/firewalld/services
firewall-cmd --permanent --add-service=lorawan-forwarder
firewall-cmd --reload
```
