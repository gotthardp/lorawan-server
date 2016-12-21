Open-source LoRaWAN Server that integrates both the network-server and the application-server. This is useful for application providers that operate their own LoRa network or for device and application developers.

The server:
 * Communicates with remote LoRaWAN gateways.
 * Performs all required encryption and integrity checks.
 * Invokes internal modules with application logic.
 * Invokes external applications connected via the WebSocket protocol.
 * Supports (any number of) Class A devices.
 * Supports both the activation by personalization and the over-the-air activation.
 * Supports unconfirmed data uplink and downlink.
 * Supports the EU 868 band.
 * Supports per-node configuration of TX power and data rate (manual ADR).
 * Runs on all major operating systems, including Windows, Linux, OS X and Solaris,
   even in a Docker container.
 * Is free, distributed under the MIT license.

The server aims to be an all-in-one software package for small private LoRa networks.
However:
 * You still need to buy your LoRaWAN Gateway.
 * You will need to deploy and maintain it yourself. (With my support.)
 * It will probably never support the sophisticated management features of the commercial-grade network-servers.

### Support or Contact
Please view the [Github page](https://github.com/gotthardp/lorawan-server) for more information.