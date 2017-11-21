# Infrastructure Administration

## Servers

Shows a single line describing the current server instance:
 * *Node* name, which is useful for [debugging](Development.md#debugging)
 * *Version* of the server
 * *Free Memory* and *Free Disk*
 * *Alerts* that may need your attention:
   * `system_memory_high_watermark` when more than 80% of available system memory
     is allocated
   * `process_memory_high_watermark` when any Erlang process has allocated more
     than 5% of total system memory
   * `disk_almost_full` if any disk partition uses more than 80% of the available
     space


## Gateways
![alt tag](https://raw.githubusercontent.com/gotthardp/lorawan-server/master/doc/images/admin-gateway.png)

For each LoRaWAN gateway you can set and view:
 * *MAC* address of the gateway
 * *NetID* of the network. Private networks should use 000000 or 000001.
 * *SubID* in the format *HexValue*:*Length* specifies the (optional) fixed
   bits in the DevAddr (see below).
 * *TX Chain* identifies the gateway "radio chain" used for downlinks (usually 0).
   It shall correspond to a `radio_x` (e.g. `radio_0`) with `tx_enable: true`
   in gateway's `global_conf.json`.
 * *TX Power (dBm)* defines transmission power for downlinks
 * *Antenna Gain (dBi)* can be set to ensure the *TX Power* + *Antenna Gain*
   is below the maximal allowed Equivalent Isotropic Radiated Power (EIRP)
   for the given region.
 * *Group*, which is an opaque string with application-specific settings.
 * *Description* for your convenience.
 * *Location* and *Altitude* of the gateway

For the status:
 * *Alerts* that may need your attention:
   * `disconnected` when no UDP packet has been received from the gateway for
     more than 20 sec
 * *Last RX* contains a timestamp of the last received packet. A gateway is
   considered dead if it didn't sent anything for more than 60 seconds.
 * *Delays* graph shows network (LAN) delay between the gateway and the server
   measured during the [`PULL_RESP`](https://github.com/Lora-net/packet_forwarder/blob/master/PROTOCOL.TXT#L274)
   sequence. Note this requires packet_forwarder v3.0 or higher.
 * *Transmissions* graph shows how much did the gateway transmit in past hour.
   This is useful to monitor regulatory compliance.

The *NetID* and *SubID* are used to create DevAddr of OTAA devices. Each DevAddr
is composed of 7 LSB of NetID, followed by *X* *SubID* bits, followed by 25-*X*
random bits. This allows operator to define separate private sub-networks using
the same *NetID*.

The gateway power is always a minimum of *TX Power* and (max EIRP - *Antenna Gain*).

To clone an existing gateway, simply save it under a different *MAC*.


## Multicast Channels

Class B and Class C devices support multicast. Multiple devices can be configured
to listen for downlinks targeted to a given *DevAddr*, so the same frame can be
received by a group of devices. See also the [Communication](Communication.md) guide.

To define a multicast channel you need to set:
 * *DevAddr* of the channel; this must not collide with any *Node* address.
 * *Region* that determines the LoRaWAN regional parameters.
 * *Application* identifier corresponding to one of the [Applications](Applications.md) configured.
 * *Group* denotes application-specific device group or behaviour.
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


## Events

This list includes an overview of errors and warnings that have occurred during
the last 24 hours. See [Event List](Events.md) guide for more details.
