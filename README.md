<p>
 Open-source LoRaWAN Server that integrates both the network-server and the application-server. This is useful for application
 providers that operate their own LoRa network or for device and application developers.
</p>
<p>
The server:
<ul>
 <li>Implements the LoRaWAN Specification v1.0.3</li>
 <li>Communicates (any number of) with remote LoRaWAN gateways.</li>
 <li>Performs all required encryption and integrity checks.
    <ul>
    <li>Supports relaxed frame-counter check for simple ABP devices.</li>
    </ul>
 </li>
 <li>Invokes internal modules with application logic.</li>
 <li>Automatically parses well-known payload formats. It currently supports:
    <ul>
    <li>Cayenne Low Power Payload</li>
    <li>Concise Binary Object Representation (CBOR)</li>
    </ul>
 </li>
 <li>Stores uplink data directly to a MongoDB collection.</li>
 <li>Invokes external applications. It currently supports connections via:
    <ul>
    <li>WebSocket protocol</li>
    <li>HTTP/1.1 and HTTP/2 protocol (REST API)</li>
    <li>MQTT, including applications hosted in Amazon AWS IoT, IBM Watson IoT or Microsoft Azure IoT Hub.</li>
    <li>AMQP 0-9-1 to the RabbitMQ</li>
    </ul>
 </li>
 <li>Handles (any number of) Class A or Class C devices.
    <ul>
    <li>Supports both the activation by personalization and the over-the-air activation.</li>
    <li>Supports both unconfirmed and confirmed data uplink and downlink.</li>
    <li>Supports multicast to user-defined groups.</li>
    <li>Supports all regions standartized in LoRaWAN 1.0.3 Regional Parameters for Europe, US, China, Australia, Asia, South Korea, India and Russia.</li>
    </ul>
 </li>
 <li>Provides a network management interface.
    <ul>
    <li>Supports both manual and automatic configuration of TX power and data rate (ADR).</li>
    <li>Monitors the server, gateways and node health status and displays device battery and connection quality indicators.</li>
    <li>Sends health alerts via e-mail or Slack.</li>
    </ul>
 </li>
 <li>Runs on all major operating systems, including Windows, Linux, OS X and Solaris, even on embedded systems like Raspbian, mLinux and other Yocto/OpenEmbedded systems, OpenWrt or in a Docker container.</li>
 <li>Can establish Clusters for high availability.</li>
 <li>Does not crash as it's implemented in Erlang, which is designed for building fault-tolerant systems.</li>
 <li>Is free, distributed under the MIT license.</li>
</ul>
</p>
<p>
The server aims to be an all-in-one software package for small private LoRa networks.
However:
<ul>
 <li>You still need to buy your LoRaWAN Gateway.</li>
 <li>You will need to deploy and maintain it yourself. (With my support.)</li>
 <li>It will probably never support the sophisticated management features of the commercial-grade network-servers.</li>
</ul>
</p>
<h3>Support or Contact</h3>
<p>
Please view the <a href="https://github.com/gotthardp/lorawan-server">Github page</a> for more information.
</p>
