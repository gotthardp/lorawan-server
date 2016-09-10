# Compact LoRaWAN Server

Open-source LoRaWAN Server that integrates both the network-server and the application-server.
This is useful for application providers that operate their own LoRaWAN network,
or for device and application developers.

The server:
 * Communicates with remote LoRaWAN gateways. It currently supports:
   * [Packet Forwarder](https://github.com/Lora-net/packet_forwarder) used in Semtech LoRa demo kits
     and [LORANK-8](http://webshop.ideetron.nl/LORANK-8) gateways.
 * Performs all required encryption and integrity checks.
 * Invokes modules with the application logic. It provides examples for:
   * [Semtech/IMST LoRaMote](http://webshop.imst.de/loramote-lora-evaluation-tool.html)
   * [Microchip LoRa(TM) Technology Mote](http://www.microchip.com/Developmenttools/ProductDetails.aspx?PartNO=dm164138)
 * Supports (any number of) Class A devices.
 * Supports both the activation by personalization and the over-the-air activation.
 * Supports unconfirmed data uplink and downlink.
 * Supports the EU 868 band.
 * Runs on all major operating systems, including Windows, Linux, OS X and Solaris,
   even in a Docker container.
 * Is free, distributed under the MIT license.

The server aims to be an all-in-one software package for small private LoRaWAN networks.
However:
 * You still need to buy your LoRaWAN Gateway.
 * You will need to deploy and maintain it yourself. (With my support.)
 * It will probably never support the sophisticated management features of the
   commercial-grade network-servers.

Let me know if you (intend to) use the lorawan-server. The API may change and some
functions may not be implemented. I will gladly assist you. Please
[add an Issue](https://github.com/gotthardp/lorawan-server/issues/new)
if you find a bug or miss a feature.


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

### Configuration of the packet_forwarder

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

## Usage

Run the lorawan-server release by:
```bash
cd lorawan-server
bin/lorawan-server
```

You can administrate and manage the server via a set of web-pages or via a REST API.
By default, the server listens on HTTP port 8080 and expects "admin" as both username and password.

### REST API

The following REST resources are made available:

  Resource        | Methods          | Explanation
 -----------------|------------------| ------------------------------------------------
  /applications   | GET              | Supported LoRaWAN applications
  /users          | GET, POST        | Users of the admin interface
  /users/*ABC*    | GET, PUT, DELETE | User *ABC*
  /gateways       | GET, POST        | LoRaWAN gateways
  /gateways/*123* | GET, PUT, DELETE | Gateway with MAC=*123*
  /devices        | GET, POST        | Devices registered for over-the-air activation
  /devices/*123*  | GET, PUT, DELETE | Device with DevEUI=*123*
  /links          | GET, POST        | Activated devices
  /links/*123*    | GET, PUT, DELETE | Activated device with DevAddr=*123*

### Web Admin

The management web-pages are available under `/admin`. It is just a wrapper around
the REST API.

To register a new gateway, create a new *Gateways* list entry.

To add a personalized device, create a new *Links* list entry.
To add an OTAA device, create a new *Devices* list entry and start the device. The *Links*
list will be updated automatically once the device joins the network.

![alt tag](https://raw.githubusercontent.com/gotthardp/lorawan-server/master/doc/admin.png)


## Development

The lorawan-server is designed to be highly extensible. I encourage you to
[Learn You Some Erlang](http://learnyousomeerlang.com/introduction) and develop
your own applications.

### Custom application handlers

To implement a new application you need to create a new `lorawan_application_xxx.erl` module
and register it in the
[`lorawan_application.erl`](https://github.com/gotthardp/lorawan-server/blob/master/src/lorawan_application.erl).

Your module just needs to export a `handle/5` function for data processing.

```erlang
handle(DevAddr, my_app, AppID, PortIn, DataIn) ->
    %% application logic
    %% ...
    {send, PortOut, DataOut}.
```

### Build Instructions

You will need the following prerequisites:
 * Rebar3, the Erlang build tool. Please follow the [installation instructions](https://www.rebar3.org/docs/getting-started).
 * npm, the JavaScript package manager.
   * On Linux, try typing `yum install npm` or `apt-get install npm`.
   * On Windows, install the [Node.js](https://nodejs.org/en/).

Then build and release the lorawan-server by:
```bash
git clone https://github.com/gotthardp/lorawan-server.git
cd lorawan-server
rebar3 release
```

The release will be created in `lorawan-server/_build/default/rel/lorawan-server`.

## Copyright and Licensing

The MIT License (MIT)

Copyright (c) 2016 Petr Gotthard

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
