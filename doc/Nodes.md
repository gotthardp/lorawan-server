# Node Administration
![alt tag](https://raw.githubusercontent.com/gotthardp/lorawan-server/master/doc/images/admin-link-status.png)

## General

Nodes are active devices. For each network node you can set:
 * *DevAddr* of the node
 * *Region* that determines the LoRaWAN regional parameters
 * *Application* identifier corresponding to one of the [Applications](Applications.md) configured.
 * *Group* denotes application-specific device group or behaviour.
 * *Arguments*, which is an opaque string with application-specific settings.
 * *NwkSKey* and *AppSKey*
 * *FCnt Check* to be used for this device (see the Devices section for more explanation).

The *Downlinks* table lists frames created by the application, which are scheduled for
transmission. Class A devices listen for downlinks only for 2 seconds after an uplink
transmission, so it may take a while until all messages are transmitted.


## ADR

Optionally, you can also set the [ADR](ADR.md) parameters. The server will attempt
to configure the device accordingly.

Below the configuration options you can monitor the performance of the node. You
can see the assumed ADR parameters and two graphs that display the last 50 received
frames.


## Status

Device status indicates the recent device *Battery* level (0-255) and the
Signal-to-Noise-Ratio (*SNR*) of received downlinks.

The *Status Time* and *Status FCnt* indicate when was the status last
received by the server.

The server requests the device status upon join or reset and then at given
time/fcnt intervals defined by the `devstat_gap` parameter. By default
`{devstat_gap, {86400, 24}}`, which requests the status every 24 hours or
every 24 frames (whatever occurs first).

The server also requests device status when the ADR parameters change.
