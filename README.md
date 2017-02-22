# Compact server for private LoRa networks

Open-source LoRaWAN Server that integrates both the network-server and the application-server.
This is useful for application providers that operate their own LoRa network,
or for device and application developers.

The server:
 * Communicates with (any number of) remote LoRaWAN gateways. It currently supports:
   * All gateways based on the [Packet Forwarder](https://github.com/Lora-net/packet_forwarder),
     such as the Semtech LoRa demo kit,
     [LoRa Lite Gateway](https://wireless-solutions.de/products/long-range-radio/lora_lite_gateway.html),
     [LORANK-8](http://webshop.ideetron.nl/LORANK-8),
     or [MultiConnect Conduit](http://www.multitech.com/brands/multiconnect-conduit).
 * Performs all required encryption and integrity checks.
 * Invokes internal modules with application logic. It provides examples for:
   * [Semtech/IMST LoRaMote](http://webshop.imst.de/loramote-lora-evaluation-tool.html)
   * [Microchip LoRa(TM) Technology Mote](http://www.microchip.com/Developmenttools/ProductDetails.aspx?PartNO=dm164138)
 * Invokes external applications. It currently supports connections via:
   * WebSocket protocol [RFC6455](https://tools.ietf.org/rfc/rfc6455.txt)
 * Supports (any number of) Class A devices.
 * Supports both the activation by personalization and the over-the-air activation.
 * Supports both unconfirmed and confirmed data uplink and downlink.
 * Supports the EU 863-870, US 902-928, CN 779-787, EU 433, AU 915-928 and CN 470-510 band.
 * Supports per-node configuration of TX power and data rate (manual ADR).
 * Runs on all major operating systems, including Windows, Linux, OS X and Solaris,
   even on embedded systems like OpenWrt or in a Docker container.
 * Does not crash as it's implemented in [Erlang](https://www.erlang.org/), which is
   designed for building fault-tolerant systems.
 * Is free, distributed under the MIT license.

The server aims to be an all-in-one software package for small private LoRa networks.
However:
 * You still need to buy your LoRaWAN Gateway.
 * You will need to deploy and maintain it yourself. (With my support.)
 * It will probably never support the sophisticated management features of the
   commercial-grade network-servers.

Let me know if you (intend to) use the lorawan-server. The API may change and some
functions may not be implemented. I will gladly assist you. Please
[add an Issue](https://github.com/gotthardp/lorawan-server/issues/new)
if you find a bug or miss a feature.


## Documentation

The lorawan-server includes all functions required to run a private LoRa network.
You only need to install the [Erlang/OTP](http://www.erlang.org) 18 or later.

The main components of the lorawan-server are shown in the following figure:

![alt tag](https://raw.githubusercontent.com/gotthardp/lorawan-server/master/doc/images/server-architecture.png)

### Usage

The [Installation Instructions](doc/Installation.md) describe how to build,
install and configure the server. You can use a Debian package, download the binary
release and run it manually or build the server from source codes.

Run the lorawan-server release by:
```bash
cd lorawan-server
bin/lorawan-server
```

You can administrate and manage the server via a set of web-pages or via a REST API
as described in the [Administration Guide](doc/Administration.md) and in the
[Adaptive Data Rate (ADR) Guide](doc/ADR.md).

After the installation you have to:
 * Add LoRaWAN gateways you want to use.
 * Configure each device you want to use, either as a personalized device (ABP) or
   as an over-the-air activated (OTAA) device.

Use the `dbexport` script to backup your list of users, gateways, devices and links.
This will create several `db*.json` files. Use the `dbimport` script to write these
files back to the server database.

The database is stored in the `Mnesia.lorawan@localhost` directory. To upgrade
the database structure or recover from database errors you should do `dbexport`,
then shutdown the server, update the server binaries, delete the Mnesia directory,
start the server and do `dbimport`.

### Integration

You can integrate lorawan-server with external applications using the WebSocket
interface as described in the [WebSocket Guide](doc/WebSockets.md).

You can also use the internal web server and develop application modules, which
may offer custom REST APIs. See the [Handler Development Guide](doc/Handlers.md).

### Development
[![Build Status](https://travis-ci.org/gotthardp/lorawan-server.svg?branch=master)](https://travis-ci.org/gotthardp/lorawan-server)

The lorawan-server is designed to be highly extensible. I encourage you to
[Learn You Some Erlang](http://learnyousomeerlang.com/introduction) and develop
your own applications.

To implement a new application you need to create a new module implementing the
`lorawan_application` behaviour as described in the [Handler Development Guide](doc/Handlers.md).


## Copyright and Licensing

The MIT License (MIT)

Copyright (c) 2016-2017 Petr Gotthard

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
