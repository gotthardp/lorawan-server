# Compact server for private LoRaWAN networks

Open-source LoRaWAN Server that integrates both the network-server and the application-server.
This is useful for application providers that operate their own LoRaWAN network,
or for device and application developers.

**Warning** After a major version upgrade you are required to review and complete
the configuration before connecting any gateway or device!
 * Migrating from version 0.4.x to 0.5.x will preserve the Device/Node addresses
   and security keys, but will delete many ADR parameters, which got moved to the
   Profile settings.
 * After migrating from version 0.5.x to 0.6.x you need to assign Profiles to
   Groups and Gateways to Areas.

The server:
 * Implements the LoRaWAN Specification v1.0.3
 * Communicates with (any number of) remote LoRaWAN gateways. It currently supports:
   * All gateways based on the [Packet Forwarder](https://github.com/Lora-net/packet_forwarder),
     such as the Semtech LoRa demo kit,
     [LoRa Lite Gateway](https://wireless-solutions.de/products/long-range-radio/lora_lite_gateway.html),
     [LORANK-8](http://webshop.ideetron.nl/LORANK-8),
     [MultiConnect Conduit](http://www.multitech.com/brands/multiconnect-conduit),
     or [Kerlink Wirnet Stations](http://www.kerlink.fr/en/products/lora-iot-station-2/wirnet-station-868)
 * Performs all required encryption and integrity checks.
   * Supports relaxed frame-counter check for simple ABP devices.
 * Invokes internal modules with application logic. It provides examples for:
   * [Semtech/IMST LoRaMote](http://webshop.imst.de/loramote-lora-evaluation-tool.html)
   * [Microchip LoRa(TM) Technology Mote](http://www.microchip.com/Developmenttools/ProductDetails.aspx?PartNO=dm164138)
     (via an [external plug-in](https://github.com/gotthardp/lorawan-server-demoapp))
 * Automatically parses well-known payload formats. It currently supports:
   * [Cayenne Low Power Payload](https://github.com/myDevicesIoT/cayenne-docs/blob/master/docs/LORA.md#cayenne-low-power-payload)
 * Stores uplink data directly to a MongoDB collection.
 * Invokes external applications. It currently supports connections via:
   * WebSocket protocol [RFC6455](https://tools.ietf.org/rfc/rfc6455.txt)
   * HTTP/1.1 and HTTP/2 protocol (REST API)
   * [MQTT v3.1/v3.1.1](http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/os/mqtt-v3.1.1-os.html),
     including applications hosted in
     [Amazon AWS IoT](https://aws.amazon.com/iot/),
     [IBM Watson IoT Platform](https://www.ibm.com/cloud-computing/bluemix/internet-of-things),
     [MathWorks ThingSpeak](https://thingspeak.com/),
     [Microsoft Azure IoT Hub](https://azure.microsoft.com/en-us/services/iot-hub/),
     [ThingsBoard Open-source IoT Platform](https://thingsboard.io),
     or [Adafruit IO](https://io.adafruit.com/)
   * AMQP 0-9-1 to the [RabbitMQ](http://www.rabbitmq.com/)
 * Handles (any number of) Class A or Class C devices.
   * Supports both the node activation by personalization (ABP) and the
     over-the-air activation (OTAA).
   * Supports both unconfirmed and confirmed data uplink and downlink.
   * Supports multicast to user-defined groups.
   * Supports all regions standartized in LoRaWAN 1.0.3 Regional Parameters for
     Europe, US, China, Australia, Asia, South Korea, India and Russia.
 * Provides a network management interface.
   * Monitors the server, gateways and node health status and displays device
     battery and connection quality indicators.
   * Supports both manual and automatic configuration of TX power and data rate (ADR).
 * Runs on all major operating systems, including Windows, Linux, OS X and Solaris,
   even on [embedded systems](doc/Embedded.md) like Raspbian, mLinux and other
   Yocto/OpenEmbedded systems, OpenWrt or in a [Docker container](doc/Docker.md).
 * Does not crash as it's implemented in [Erlang](https://www.erlang.org/), which is
   designed for building fault-tolerant systems.
 * Is free, distributed under the MIT license.

The server aims to be an all-in-one software package for small private LoRaWAN
networks. However:
 * You still need to buy your LoRaWAN Gateway.
 * You will need to deploy and maintain it yourself. (With my support.)
 * It will probably never support the sophisticated management features of the
   commercial-grade network-servers.

The maximum amount of gateways, devices and nodes the server can support depends
on the server load and hardware performance. There are no hard limits.

The API may change and some functions may not be implemented.
To ask questions or request features please join the
[lorawan-server mailing list](https://groups.google.com/forum/#!forum/lorawan-server).
We will gladly assist you. If you find a bug, you may also
[add an Issue](https://github.com/gotthardp/lorawan-server/issues/new).


## Documentation

The lorawan-server includes all functions required to run a private LoRaWAN network.
It integrates your LoRaWAN network directly with your backend IT systems.
The server is provided as a comprehensive package with a single configuration file
and a single administration tool.
You only need to install the [Erlang/OTP](http://www.erlang.org) 19 or 20.
**Erlang 21 is not yet supported.**

The main components of the lorawan-server are shown in the following figure:

![alt tag](https://raw.githubusercontent.com/gotthardp/lorawan-server/master/doc/images/system-architecture.png)

### Usage

The server behaviour is described in the [Introduction](doc/Introduction.md).

The [Installation Instructions](doc/Installation.md) describe how to build,
install and configure the server. You can use a Debian package, download the binary
release and run it manually or build the server from source codes.

Run the lorawan-server release by:
```bash
cd lorawan-server
bin/lorawan-server
```

Don't forget to set the server address and port (by default 1680) in the LoRaWAN
gateways you want to use with the server.

You can terminate the lorawan-server by:
```bash
bin/lorawanctl stop
```

You can administrate and manage the server via a set of web-pages or via a REST API
as described in the [Administration Guide](doc/Administration.md). By default you
can access the administration at http://*server*:8080, using "admin" as both
username and password. After the installation you have to:
 * Change the default password to something more secure.
 * Set parameters of your **Network** and add LoRaWAN **Gateways** you want to use.
 * Define the device **Profiles**, one for each device type that you will have.
 * Configure each device you want to use, either as a personalized **Node** (ABP)
   or as an **Commissioned** and over-the-air activated (OTAA) device.

### Integration

You can integrate lorawan-server with external applications using Backend
[Handlers](doc/Handlers.md) and [Connectors](doc/Connectors.md). Instructions on
how to integrate with some major clouds such as AWS or Azure are provided in the
[Integration Guide](doc/Integration.md).

You can also use the internal web server and develop internal applications, which
may offer custom REST APIs. The lorawan-server is designed to be highly extensible.
I encourage you to [Learn You Some Erlang](http://learnyousomeerlang.com/introduction)
and develop your own modules.

To implement an internal application you need to create a new module implementing the
`lorawan_application` behaviour as described in the
[Custom Application Guide](doc/Applications.md) and [Development Guide](doc/Development.md).

### Troubleshooting
[![Build Status](https://travis-ci.org/gotthardp/lorawan-server.svg?branch=master)](https://travis-ci.org/gotthardp/lorawan-server)

First of all, please read the documentation.

If the server doesn't do what you expect, please review the server logs and consult the
[Troubleshooting Instructions](doc/Troubleshooting.md) for the most common problems.

If the problem persists, please verify you have the latest version. I recommend
to always use the [latest release](https://github.com/gotthardp/lorawan-server/releases).
If you use the [latest sources](https://github.com/gotthardp/lorawan-server/commits/master),
please verify the "build" icon above is green and then try upgrading by running:

```bash
cd lorawan-server
git pull
make upgrade
make release
```

If the "build" icon above is red, please wait few minutes (or hours) until it
gets green again.

If nothing helps, please contact the
[lorawan-server mailing list](https://groups.google.com/forum/#!forum/lorawan-server)
or review the existing
[issues](https://github.com/gotthardp/lorawan-server/issues) to verify the
problem was not already reported and then
[create new issue](https://github.com/gotthardp/lorawan-server/issues/new).

### Public References

The server is used (both commercially and non-commercially) by various companies
and institutions. It was mentioned by the following blogs and articles:
 * [Three reasons for creating an Open Source LoRaWan server](http://research.konicaminolta.eu/three-reasons-for-creating-an-open-source-lorawan-server)
 * [LoraWAN server running on OpenWrt/LEDE](http://matchx.io/community/box/5-lorawan-server-running-on-the-box)
 * [Espruino RN2483 LoRa Modules](http://www.espruino.com/RN2483)
 * [1-Gate LoRaWAN Gateway COMPACT](http://www.1-gate.com/english/lorawan-gateways)
 * [LoRaWAN evaluation by Witekio](https://witekio.com/blog/lorawan-dedicated-iot-network)

Please let me know if you use the lorawan-server and want to be listed here.


## Copyright and Licensing

The lorawan-server is distributed under the terms of the MIT License.
See the [LICENSE](LICENSE).

Copyright (c) 2016-2018 Petr Gotthard

### Sponsors

<a href="https://www.konicaminolta.eu/en/business-solutions/home.html"><img align="left" src="https://raw.githubusercontent.com/gotthardp/lorawan-server/master/doc/images/logo-konica-minolta.png"></a>
[KMLE](http://research.konicaminolta.eu) is working on the challenge of
helping customers optimize the way they work by digitizing the workplace
and their workflows.

<br/>

<a href="http://www.iotini.com"><img align="left" src="http://www.iotini.com/images/logo.png"></a>
[I2OT](http://www.iotini.com/#product) is the first industrial wireless sensor
system that provides a total solution for industrial sensing needs.
