# Server Installation

This document describes how to build, install and configure the lorawan-server.

## Installation

### Using the Debian package

On the Debian Linux and its clones like Raspbian you can use the .deb package.

Download the Debian package
[lorawan-server-<VERSION>.deb](https://github.com/gotthardp/lorawan-server/releases)
and install it by:
```bash
dpkg -i lorawan-server-<VERSION>.deb
```

Then start the server by `systemctl start lorawan-server`.

### Using the Binary Release on Linux

You will need the Erlang/OTP 18 or later. Try typing `yum install erlang` or
`apt-get install erlang`.

Then download the latest binary release
[lorawan-server-<VERSION>.tar.gz](https://github.com/gotthardp/lorawan-server/releases)
and (on Linux) unpack it by:
```bash
mkdir lorawan-server
mv lorawan-server-<VERSION>.tar.gz lorawan-server/
cd lorawan-server
tar -zxvf lorawan-server-<VERSION>.tar.gz
```

You can run the server by:
```bash
bin/lorawan-server
```

The lorawan-server can be started in background as a daemon.
On Linux systems with systemd you should:
 * Unpack the binary release to `/usr/lib/lorawan-server`
 * Copy `bin/lorawan-server.service` to `/lib/systemd/system`
 * Create a dedicated user by `useradd --home-dir /var/lib/lorawan-server --create-home lorawan`
 * Start the server by `systemctl start lorawan-server`

### Using the Binary Release on Windows

Install the [32-bit or 64-bit Binary File](http://www.erlang.org/downloads) of
Erlang/OTP 18 or later.

Unpack the release using the [7-Zip](http://www.7-zip.org) and run the server by
```bash
bin/lorawan-server.bat
```

You can also run the lorawan-server as a Windows service.
The service is managed using `bin/lorawan-service.bat` *command*, where:
 * *add* will add the service. Once added you can use the standard Windows control
   panel administrative tools to start/stop or enable/disable the service.
 * *remove* will remove the previously added service.
 * *list* will display parameters of a previously added service.

## Server Configuration

Review the `lorawan-server/releases/<VERSION>/sys.config` with the server configuration:
 * To enable/disable applications, modify the `plugins` section. For more details
   see the [Handler Development Guide](Handlers.md).

For example:
```erlang
[{lorawan_server, [
    % update this list to add/remove applications
    {plugins, [
        {<<"semtech-mote">>, lorawan_application_semtech_mote},
        {<<"microchip-mote">>, lorawan_application_microchip_mote},
        {<<"websocket">>, lorawan_application_websocket}]},
    % UDP port listening for packets from the packet_forwarder Gateway
    {forwarder_port, 1680},
    % HTTP port for web-administration and REST API
    {http_admin_port, 8080},
    % default username and password for the admin interface
    {http_admin_credentials, {<<"admin">>, <<"admin">>}}
]}].
```

Review the `lorawan-server/lib/lorawan_server-<VERSION>/priv/admin/admin.js` with the
admin configuration:
 * You may need to obtain a [Google API](https://console.developers.google.com) key for
   the Google Maps and enter it in `GoogleMapsKey`. For deployments on a local network
   this is not needed.

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

Get the latest sources by:
```bash
git clone https://github.com/gotthardp/lorawan-server.git
cd lorawan-server
```

If you already obtained the sources you can upgrade to the latest version by:
```bash
cd lorawan-server
git pull
make upgrade
```

Then build and release the lorawan-server by:
```bash
make release
```

The release will be created in `lorawan-server/_build/default/rel/lorawan-server`.
