<p>
 Open-source LoRaWAN Server that integrates both the network-server and the application-server. This is useful for application
 providers that operate their own LoRa network or for device and application developers.
</p>
<p>
The server:
<ul>
 <li>Implements the LoRaWAN Specification v1.0.1</li>
 <li>Communicates (any number of) with remote LoRaWAN gateways.</li>
 <li>Performs all required encryption and integrity checks.
    <ul>
    <li>Supports relaxed frame-counter check for simple ABP devices.</li>
    </ul>
 </li>
 <li>Invokes internal modules with application logic.</li>
 <li>Invokes external applications. It currently supports connections via:
    <ul>
    <li>WebSocket protocol</li>
    <li>MQTT, including applications hosted in Amazon AWS IoT, IBM Watson IoT or Microsoft Azure IoT Hub.</li>
    </ul>
 </li>
 <li>Handles (any number of) Class A or Class C devices.
    <ul>
    <li>Supports both the activation by personalization and the over-the-air activation.</li>
    <li>Supports both unconfirmed and confirmed data uplink and downlink.</li>
    <li>Supports multicast to user-defined groups.</li>
    <li>Supports EU 863-870, US 902-928, CN 779-787, EU 433, AU 915-928, CN 470-510, KR 920-923 and AS923-JP bands.</li>
    </ul>
 </li>
 <li>Provides a network management interface.
    <ul>
    <li>Monitors the server, gateways and node health status and displays device battery and connection quality indicators.</li>
    <li>Supports both manual and automatic configuration of TX power and data rate (ADR).</li>
    </ul>
 </li>
 <li>Runs on all major operating systems, including Windows, Linux, OS X and Solaris, even on embedded systems like Raspbian, mLinux and other Yocto/OpenEmbedded systems, OpenWrt or in a Docker container.</li>
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
<p>
 The maximum amount of gateways, devices and nodes the server can support depends on the server load and hardware performance.
 There are no hard limits.
</p>
<h3>Support or Contact</h3>
<p>
Please view the <a href="https://github.com/gotthardp/lorawan-server">Github page</a> for more information.
</p>
