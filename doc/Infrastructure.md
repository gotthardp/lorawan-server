# Infrastructure Administration

## Gateways
![alt tag](https://raw.githubusercontent.com/gotthardp/lorawan-server/master/doc/images/admin-gateway.png)

For each LoRaWAN gateway you can set and view:
 * *MAC* address of the gateway
 * *NetID* of the network
 * *TX Chain* identifies the gateway "RF chain" used for downlinks; usually 0
 * *TX Power (dBm)* defines transmission power for downlinks
 * *Antenna Gain (dBi)* can be set to ensure the *TX Power* + *Antenna Gain*
   is below the maximal allowed Equivalent Isotropic Radiated Power (EIRP)
   for the given region.
 * *Description* for your convenience.
 * *Last RX* contains a timestamp of the last received packet. A gateway is
   considered dead if it didn't sent anything for more than 60 seconds.
 * *Location* and *Altitude* of the gateway

The gateway power is always a minimum of *TX Power* and (max EIRP - *Antenna Gain*).


## Multicast Channels

Class B and Class C devices support multicast. Multiple devices can be configured
to listen for downlinks targeted to a given *DevAddr*, so the same frame can be
received by a group of devices.

To define a multicast channel you need to set:
 * *DevAddr* of the channel; this must not collide with any *Node* address.
 * *Region* that determines the LoRaWAN regional parameters.
 * *Application* identifier corresponding to one of the [Applications](Applications.md) configured.
 * *Group* denotes application-specific device group or behaviour.
 * *Channel* determines the frequency.
 * *Data rate* and *Coding rate* determine the radio signal encoding.
 * *NwkSKey* and *AppSKey*
 * *Gateway* indicates MAC of the gateway that shall transmit the broadcast
 * *FCnt Down* is the broadcast frame counter.


## Ignored Nodes

If two networks operate in the same area the server will receive frames from
devices using the other network. Each time such device sends a frame, the
`unknown_devaddr` error will be displayed.

To suppress warnings from a specific device, enter its *DevAddr* and
`FFFFFFFF` as the *Mask*.

To suppress warnings from all devices in a given network, enter a *DevAddr* of
one device and `FE000000` as the *Mask*. The 7 upper bits (MSB) match in the
address match the 7 lower bits (LSB) in the network identifier.
