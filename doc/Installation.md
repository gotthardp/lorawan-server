# Server Installation

This document describes how to build, install and configure the lorawan-server.

## Installation

You will need the Erlang/OTP 18 or later.
 * On Linux, try typing `yum install erlang` or `apt-get install erlang`.
 * On Windows, install the [32-bit or 64-bit Binary File](http://www.erlang.org/downloads).

Then download the binary release
[lorawan-server-0.1.0.tar.gz](https://github.com/gotthardp/lorawan-server/releases/download/v0.1.0/lorawan-server-0.1.0.tar.gz)
and unpack it by:
```bash
mkdir lorawan-server
mv lorawan-server-0.1.0.tar.gz lorawan-server/
cd lorawan-server
tar -zxvf lorawan-server-0.1.0.tar.gz
```

## Server Configuration

Review the `lorawan-server/releases/0.1.0/sys.config` with the server configuration.
For example:
```erlang
[{lorawan_server, [
    % UDP port listening for packets from the packet_forwarder Gateway
    {forwarder_port, 1680},
    % HTTP port for web-administration and REST API
    {http_admin_port, 8080},
    % default username and password for the admin interface
    {http_admin_credentials, {<<"admin">>, <<"admin">>}}
]}].
```

You may need to enable communication channels from LoRaWAN gateways in your firewall.
If you use the `firewalld` (Fedora, RHEL, CentOS) do:
```bash
cp lorawan-forwarder.xml /usr/lib/firewalld/services
firewall-cmd --permanent --add-service=lorawan-forwarder
firewall-cmd --reload
```

## Configuration of the packet_forwarder

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

## Build Instructions

You will need the following prerequisites:
 * Rebar3, the Erlang build tool.
   * On Linux it will download automatically.
   * On Windows follow the [installation instructions](https://www.rebar3.org/docs/getting-started).
 * npm, the JavaScript package manager.
   * On Linux, try typing `yum install npm` or `apt-get install npm`.
   * On Windows, install the [Node.js](https://nodejs.org/en/).

Then build and release the lorawan-server by:
```bash
git clone https://github.com/gotthardp/lorawan-server.git
cd lorawan-server
make release
```

The release will be created in `lorawan-server/_build/default/rel/lorawan-server`.
