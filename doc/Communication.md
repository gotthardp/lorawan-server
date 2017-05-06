# Communication

This document describes how the lorawan-server handles LoRaWAN communication with
active network Nodes.

## Uplink

LoRaWAN gateways act like one distributed antenna. When a Node sends a
LoRaWAN frame (message) it gets received by one (or multiple) gateways. The
gateways are very simple and just forward all received frames to the server.

Upon receiving an uplink frame the server:
 * Filters duplicate frames, which were received by multiple gateways.
 * Verifies the message integrity check and decrypts application data.
 * Compares the received Frame Counter (*FCnt Up*) with the previously received
   *FCnt Up* to identify retransmissions:
   * If the *FCnt Up* increased the respective [Application Handler](Applications.md)
     gets invoked.
   * If the *FCnt Up* is the same the device just retransmitted the previous frame.
     The frame gets logged in the **Received Frames** list, but handler is not invoked.
   * If the *FCnt Up* decreased an error [Event](Events.md) is generated, unless
     the *FCnt Up* did reset and the Node *FCnt Check* is set to *Reset on zero*.
     Such frames are processed as if the *FCnt Up* increased.


## Downlink

LoRaWAN devices can be Class A, B or C.

### Class A

Class A devices listen for downlink frames only for few seconds after sending an
uplink frame. This is the default and most common behaviour.

Upon receiving a downlink frame for a given Node:
 * When the Node just sent an uplink frame the server sends the downlink frame
   immediately to the closest Gateway, which forwards the frame to the device.
 * When the Node did not send anything the frame gets queued in the server
   and waits for the Node to send a next uplink. The queued *Downlinks* can
   be viewed via the [Node Administration](Nodes.md).

### Class B

Currently not supported.

### Class C

Class C devices listen for downlink frames constantly.

The server has no knowledge if a particular device is Class A or C. It assumes
the applications (somehow) know this.

When initiating a downlink applications can set requested downlink *time*. This
can be either a specific timestamp, or an `immediately` flag. See the
[JSON Payload](JSON.md) documentation for more details.

Upon receiving a downlink frame with the *time* parameter set the server sends the
frame to the gateway and relies on its scheduling mechanism to transmit the frame.

### Multicast

It is possible to send one downlink frame to multiple Class C devices. First,
a multicast channel needs to be defined both in the network
[Infrastructure](Infrastructure.md) and also in the device (see `struct sMulticastParams`
in [LoRaMac-node](https://github.com/Lora-net/LoRaMac-node)). The multicast DevAddr
is a special dedicated address that must not collide with any *Node* address.

To send a multicast downlink the application simply initiates a downlink with:
 * DevAddr that was previously defined as a multicast channel.
 * *time* parameter set to a given timestamp, or to the `immediately` flag.
