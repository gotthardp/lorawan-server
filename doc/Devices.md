# Device Administration

## Profiles

Each profile represents a group of devices that connect to the network. For each
Profile you can set:
 * *Name* of the profile.
 * *Network* for which are the devices in this profile configured.
 * *Application* the devices are using.
 * *Identifier*, which is a custom application id.
 * *Can Join?* flag that allows you to prevent the device from joining.
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

### ADR

For each device Profile you can define "desired" ADR parameters. The server will
handle these values as described in the [ADR](ADR.md) documentation.
 - **ADR Mode** determines the ADR mechanism for this device;
 - **Set Power** defines the power (in dBm) to be requested;
 - **Set Data Rate** defines the data rate;
 - **Set Channels** defines the set of channels to be used. The channels are given
   as a comma-separated list of interfaces, e.g. `0-2` (for EU) or `0-71` (for US).
 - **Set RX1 DR offset** defines the offset between the uplink and the RX1 slot
   downlink data rates
 * *Set RX2 DR* defines the data rate for the second reception slot (RX2).
 * *Set RX2 Freq* defines the default frequency in the RX2 receive window.
 * *Request Status* flag, which can be used to disable the status requests for
   simple devices that do not support it (by default true).


## Commissioned

This list contains devices that can join the LoRaWAN network using the
over-the-air activation (OTAA). The active network nodes that either have
already joined or have been activated-by-personalization (ABP) are listed on the
[Nodes](Nodes.md) page.

![alt tag](https://raw.githubusercontent.com/gotthardp/lorawan-server/master/doc/images/admin-device.png)

For each device, which may connect to your network, you can set:
 * *DevEUI* of the device
 * *Profile* that this device uses
 * *Arguments*, which is an opaque string with application-specific settings.
 * *AppEUI* and *AppKey*
 * *Last Join* is a timestamp of the last successful Join request.

The *US 902-928MHz* region allows a *Private Hybrid* mode introduced by
[Multitech](www.multitech.net/developer/software/lora/introduction-to-lora).
This is useful when you want to split the radio spectrum to 8 different sub-bands,
but it requires custom device firmware.

To clone an existing device, simply save it under a different *DevEUI*.

Once the device joins the network, the *Node* field will contain a reference to the *Nodes* list.


## Activated Nodes

This list contains active network nodes that either have already joined the network
using the over-the-air activation (OTAA) or have been activated-by-personalization
(ABP). All devices that can join the network using OTAA are listed on the
[Devices](Devices.md) list.

![alt tag](https://raw.githubusercontent.com/gotthardp/lorawan-server/master/doc/images/admin-node.png)

For each active network Node you can set:
 * *DevAddr* of the node
 * *Profile* that this Node uses
 * *Arguments*, which is an opaque string with application-specific settings.
 * *NwkSKey* and *AppSKey*
 * *FCnt Up* and *FCnt Down* frame counters

The *Downlinks* table contains frames created by the application, which are scheduled for
transmission. Class A devices listen for downlinks only for 2 seconds after an uplink
transmission, so it may take a while until all messages are transmitted. Class C
downlinks are not listed there as these are scheduled immediately.

To clone an existing node, simply save it under a different *DevAddr*.

### ADR

After a device joins the network the server requests the ADR parameters specified
in the Device ADR configuration explained above. These become the new requested
Node ADR parameters, regardless of any previous settings.

Note that the requested Node parameters are not affected by an ABP device reset.

The server administration also displays the currently used ADR settings:
 - **Used ADR** indicates whether the node can do ADR;
 - **Used Channels** indicates the set of channels used;
 - **Used RX1 DR offset** indicates the offset used;
 - **RX** graph indicates the device Power (dBm), Data Rate and Frequency (MHz);
 - **RX Quality** graph indicates the SNR (dB) and RSSI (dBm).

After a join or a reset of an ABP device the effective parameters are reverted to
their standard defaults.

The ADR request to change these settings is sent to the device only when the
**Use ADR** is `ON` and when all requested ADR settings are defined (not empty).

If the device does not support (or allow) some of the requested settings, the
entire request will fail. When a request fails, the requested ADR parameter(s) that
caused the failure will be cleared (set to empty) and no other parameter will become
effective.

Below the configuration options you can monitor the performance of the node. You
can see the assumed ADR parameters and two graphs that display the last 50 received
frames.

Note that the (Google) graphs will not be displayed when the Internet connectivity
is not available, e.g. in an isolated network.

### Status

Shows:
 - *Alerts* that may need your attention:
   * `battery_low` when the device battery is below 20% its capacity;
   * `downlink_noise` when the indicated D/L SNR is close to the sensitivity limit;
   * `many_resets` when the device sent multiple Join requests without sending
     any other frames.
 - *Status Time* and *Status FCnt* indicating when was the status last
   received by the server;
 - *Device Status* graph that shows the recent device *Battery* level (0-255)
   and the Signal-to-Noise-Ratio of received downlinks (*D/L SNR*).

The server requests the device status upon join or reset and then at given
time/fcnt intervals defined by the `devstat_gap` parameter. By default
`{devstat_gap, {432000, 96}}`, which requests the status every 5 days or
every 96 frames (whatever occurs first).

The server also requests device status when the ADR parameters change.


## Ignored Nodes

If two networks operate in the same area the server will receive frames from
devices using the other network. Each time such device sends a frame, the
`unknown_devaddr` error will be displayed.

To suppress warnings from a specific device, enter its *DevAddr* and
`FFFFFFFF` as the *Mask*.

To suppress warnings from all devices in a given network, enter a *DevAddr* of
one device and `FE000000` as the *Mask*. The 7 upper bits (MSB) match in the
address match the 7 lower bits (LSB) in the network identifier.
