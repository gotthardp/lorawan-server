# Server Installation

This document describes how to build, install and configure the lorawan-server.

## Installation

### Using the Debian package

On the Debian Linux and its clones like Raspbian you can use the .deb package.

Download the Debian package
[lorawan-server-*.deb](https://github.com/gotthardp/lorawan-server/releases)
and install it by:
```bash
dpkg -i lorawan-server-<VERSION>.deb
```

Then start the server by `systemctl start lorawan-server`.

### Using the Binary Release on Linux

You will need the Erlang/OTP 19 or later. Try typing `yum install erlang` or
`apt-get install erlang`.

If your Linux distribution includes some older version of Erlang/OTP, try installing
an update from [Erlang Solutions](https://www.erlang-solutions.com/resources/download.html).

Then download the latest binary release
[lorawan-server-*.tar.gz](https://github.com/gotthardp/lorawan-server/releases)
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

By default the database directory `Mnesia.lorawan@*` and the `log` files are
stored in the `lorawan-server` directory you created. If you want to store the
run-time information to another directory, set the `LORAWAN_HOME` environment
variable.

The lorawan-server can be started in background as a daemon.
On Linux systems with systemd you should:
 * Unpack the binary release to `/usr/lib/lorawan-server`
 * Copy `bin/lorawan-server.service` to `/lib/systemd/system`
 * Create a dedicated user by `useradd --home-dir /var/lib/lorawan-server --create-home lorawan`
 * Start the server by `systemctl start lorawan-server`

This will put the database and the logs into `/var/lib/lorawan-server`.

### Using the Binary Release on Windows

Install the [32-bit or 64-bit Binary File](http://www.erlang.org/downloads) of
Erlang/OTP 19 or later.

Unpack the binary release
[lorawan-server-*.tar.gz](https://github.com/gotthardp/lorawan-server/releases)
using the [7-Zip](http://www.7-zip.org) to a new folder
(e.g. `lorawan-server`) and then run the server by double-clicking `lorawan-server.bat`
in the `lorawan-server/bin` folder.

You can also run the lorawan-server as a Windows service.
The service can be managed from a Command Prompt (`cmd`) using
`lorawan-service.bat <command>`, where the `<command>` could be:
 * *add* to add the service. Once added you can use the standard Windows control
   panel administrative tools to start/stop or enable/disable the service.
 * *remove* to remove the previously added service.
 * *list* to display parameters of a previously added service.

### Using the Binary Release on Mac OS

Install Erlang by `brew install erlang`.

Unpack and run the binary release
```bash
mkdir lorawan-server
mv lorawan-server-<VERSION>.tar.gz lorawan-server/
cd lorawan-server
tar -zxvf lorawan-server-<VERSION>.tar.gz
bin/lorawan-server
```


## Upgrade

The server binaries are stored in three subdirectories: `bin`, `lib` and `releases`.
These files are either:
 * In `/usr/lib/lorawan-server` when using the official releases
 * In `lorawan-server/_build/default/rel/lorawan-server` when you build the server
   from sources
 * Or wherever you extracted them

The server run-time files are automatically created during the first run. It
includes the database in `Mnesia.lorawan@*` and `log` files. These files are either:
 * In `/var/lib/lorawan-server` when using the official releases
 * In the directory specified by the `LORAWAN_HOME` environment variable
 * Otherwise it is in the same directory as the server binaries

To upgrade your server binaries:
 * Stop the lorawan-server
 * Backup or make sure you don't delete the `Mnesia.lorawan@*` sub-directory.
 * Delete the existing `bin`, `lib` and `releases` sub-directories and replace
   them by `bin`, `lib` and `releases` from the new binary release. You can simply
   unpack the content of the binary release e.g. in `/usr/lib/lorawan-server`.
 * Copy `bin/lorawan-server.service` to `/lib/systemd/system`
 * Reload the services by `sudo systemctl daemon-reload`
 * Start the lorawan-server


## Server Configuration

Review the `lorawan-server/releases/<VERSION>/sys.config` with the server configuration:
 * To enable/disable applications, modify the `plugins` section. For more details
   see the [Custom Application Guide](Applications.md).

Note that during the manual installation the `sys.config` is created
automatically by the release tool (`make release`) based on the
[lorawan_server.config](/lorawan_server.config).

For example:
```erlang
[{lorawan_server, [
    % update this list to add/remove applications
    {plugins, [
        {<<"semtech-mote">>, lorawan_application_semtech_mote},
        {<<"microchip-mote">>, lorawan_application_microchip_mote},
        {<<"websocket">>, lorawan_application_websocket}]},
    % UDP port listening for packets from the packet_forwarder Gateway
    {packet_forwarder_listen, [{port, 1680}]},
    % HTTP port for web-administration and REST API
    {http_admin_listen, [{port, 8080}]},
    % default username and password for the admin interface
    {http_admin_credentials, {<<"admin">>, <<"admin">>}},
    % websocket expiration if client sends no data
    {websocket_timeout, 3600000} % ms
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

When both packet_forwarder and lorawan-server are running on the same machine
use `localhost` or `127.0.0.1` as the `server_address`.

## Build Instructions

### Manual Installation

You will need the following prerequisites:
 * Rebar3, the Erlang build tool.
   * On Linux it will download automatically.
   * On Windows follow the [installation instructions](https://www.rebar3.org/docs/getting-started).
   * On Mac OS, run `brew install rebar`.
 * npm, the JavaScript package manager.
   * On Linux, try typing `yum install npm` or `apt-get install npm`.
   * On Windows, install the [Node.js](https://nodejs.org/en/).
   * On Mac OS, run `brew install node`.

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

According to the above installation instructions the server binaries are under
`/usr/lib/lorawan-server`. To upgrade your installation you shall **replace** the
content of the `bin`, `lib` and `releases` sub-directories with the newly created
content.

### Creating the Debian package

On the Debian Linux and its clones like Raspbian you can use the .deb package.

Build the Debian package bu running `make dpkg`. It will request your `root`
password and then create a package
`lorawan-server/_build/default/rel/lorawan-server/lorawan-server_<VERSION>.deb`.

You can then install the package by:
```bash
dpkg -i lorawan-server_*.deb
```

You can start the server by `systemctl start lorawan-server`.
