# Compact server for private LoRa networks

Open-source LoRaWAN Server that integrates both the network-server and the application-server.
This is useful for application providers that operate their own LoRa network,
or for device and application developers.

The server:
 * Implements the LoRaWAN Specification v1.0.1
 * Communicates with (any number of) remote LoRaWAN gateways. It currently supports:
   * All gateways based on the [Packet Forwarder](https://github.com/Lora-net/packet_forwarder),
     such as the Semtech LoRa demo kit,
     [LoRa Lite Gateway](https://wireless-solutions.de/products/long-range-radio/lora_lite_gateway.html),
     [LORANK-8](http://webshop.ideetron.nl/LORANK-8),
     or [MultiConnect Conduit](http://www.multitech.com/brands/multiconnect-conduit).
 * Performs all required encryption and integrity checks.
   * Supports relaxed frame-counter check for simple ABP devices.
 * Invokes internal modules with application logic. It provides examples for:
   * [Semtech/IMST LoRaMote](http://webshop.imst.de/loramote-lora-evaluation-tool.html)
   * [Microchip LoRa(TM) Technology Mote](http://www.microchip.com/Developmenttools/ProductDetails.aspx?PartNO=dm164138)
 * Invokes external applications. It currently supports connections via:
   * WebSocket protocol [RFC6455](https://tools.ietf.org/rfc/rfc6455.txt)
   * [MQTT v3.1/v3.1.1](http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/os/mqtt-v3.1.1-os.html),
     including applications hosted in
     [Amazon AWS IoT](https://aws.amazon.com/iot/),
     [IBM Watson IoT Platform](https://www.ibm.com/cloud-computing/bluemix/internet-of-things),
     [MathWorks ThingSpeak](https://thingspeak.com/),
     [Microsoft Azure IoT Hub](https://azure.microsoft.com/en-us/services/iot-hub/)
     or [Adafruit IO](https://io.adafruit.com/)
 * Handles (any number of) Class A or Class C devices.
   * Supports both the node activation by personalization (ABP) and the over-the-air activation (OTAA).
   * Supports both unconfirmed and confirmed data uplink and downlink.
   * Supports multicast to user-defined groups.
   * Supports EU 863-870, US 902-928, CN 779-787, EU 433, AU 915-928, CN 470-510 and KR 920-923 band.
 * Provides a network management interface.
   * Displays device battery and connectivity status.
   * Supports both manual and automatic configuration of TX power and data rate (ADR).
 * Runs on all major operating systems, including Windows, Linux, OS X and Solaris,
   even on embedded systems like OpenWrt or in a [Docker container](doc/Docker.md).
 * Does not crash as it's implemented in [Erlang](https://www.erlang.org/), which is
   designed for building fault-tolerant systems.
 * Is free, distributed under the MIT license.
