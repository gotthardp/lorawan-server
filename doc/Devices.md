# Device Administration

This list contains devices that can join the LoRaWAN network using the
over-the-air activation (OTAA). The active network nodes that either have
already joined or have been activated-by-personalization (ABP) are listed on the
[Nodes](Nodes.md) page.

![alt tag](https://raw.githubusercontent.com/gotthardp/lorawan-server/master/doc/images/admin-device.png)

## General

For each device, which may connect to your network, you can set:
 * *DevEUI* of the device
 * *Region* that determines the LoRaWAN regional parameters
 * *Application* identifier corresponding to one of the [Applications](Applications.md) configured.
 * *Group* denotes application-specific device group or behaviour.
 * *Arguments*, which is an opaque string with application-specific settings.
 * *AppEUI* and *AppKey*
 * *FCnt Check* to be used for this device
   * *Strict 16-bit* (default) or *Strict 32-bit* indicate a standard compliant counter.
   * *Reset on zero* behaves like a "less strict 16-bit", which allows personalised (ABP)
     devices to reset the counter.
     This weakens device security a bit as more reply attacks are possible.
   * *Disabled* disables the check for faulty devices.
     This destroys the device security.
 * *TX Window* to be used for downlinks to this device:
   * *Auto* to choose the earliest feasible option, either RX1 or RX2
   * *RX1* to always use RX1
   * *RX2* to always use RX2
 * *Can Join?* flag that allows you to prevent the device from joining.
 * *Last Join* is a timestamp of the last successful Join request.

The *US 902-928MHz* region allows a *Private Hybrid* mode introduced by
[Multitech](www.multitech.net/developer/software/lora/introduction-to-lora).
This is useful when you want to split the radio spectrum to 8 different sub-bands,
but it requires custom device firmware.

To clone an existing device, simply save it under a different *DevEUI*.

Once the device joins the network, the *Node* field will contain a reference to the *Nodes* list.


## ADR

Optionally, you can also define a set of [ADR](ADR.md) parameters. Once the device
joins the network, the server will attempt to configure the device accordingly.


## Status

You can set:
 - *Request Status* flag, which can be used to disable the status requests for
   simple devices that do not support it (by default true).
