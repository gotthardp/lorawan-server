# Adaptive Data Rate (ADR)

Transmissions (uplinks) of each node can use different transmission power, data
rate and channels (frequencies). Depending on the *ADR Mode* this can either be
manually defined by the administrator or automatically determined by the server.

Note this functionality must be also enabled in the device, which is indicated
by the *Use ADR* flag.

![alt tag](https://raw.githubusercontent.com/gotthardp/lorawan-server/master/doc/images/admin-adr.png)


## ADR Modes

### Disabled

This mode disables the ADR, regarless the *Use ADR* flag. The server will never
attempt to modify the Power, Data Rates or Channels. Changes to the requested
ADR parameters have no effect.

### Auto-Adjust (Automatic ADR)

In this mode the server automatically adjusts the ADR parameters depending on
the uplink signal quality.

The server calculates an average RSSI and SNR for the last 20 frames received.
 * The *Data Rate* (Spreading Factor) is defined so that the LoRa demodulator SNR
   for the target *Data Rate* matches the observed average uplink SNR.
 * When the highest *Data Rate* is reached, the *Power* is decreased, so the
   RSSI stays above -100 dbB, which is an expected sensitivity for the highest rate.

Manual changes to the requested *Power* and *Data Rate* parameters will have no
effect. The server will set these values automatically after receiving at least
20 frames with steady ADR parameters.

When the device decreases the used *Data Rate*, the 20 frames buffer gets flushed
and the server will wait for 20 frames more before any adjustment will be made.

### Maintain

In this mode the server maintains for the given node the requested ADR
parameters. The device is not allowed to use a different *Data Rate*.

If the user changes an ADR parameter, the ADR request will be sent to the node
with the next downlink frame.

If the device decreases the used Data Rate, the server will send an ADR request
to set this back to the requested value.

### Set, then Auto-Adjust

The server will enforce the requested ADR parameters by sending an ADR request
with the next downlink frame and then switch to the *Auto-Adjust mode*.

The ADR request will be sent even if the requested ADR parameters are the same
as those already used.

### Set, then Disable

The server will enforce the requested ADR parameters by sending an ADR request
with the next downlink frame and then switch to the *Disabled mode*.

The ADR request will be sent even if the requested ADR parameters are the same
as those already used.


## Device Configuration

For each OTAA device you can define what ADR parameters shall be requested when
the device joins the network:
 - **ADR Mode** determines the ADR mechanism for this device;
 - **Set Power** defines the power (in dBm) to be requested;
 - **Set Data Rate** defines the data rate;
 - **Set Channels** defines the set of channels to be used. The channels are given
   as a comma-separated list of interfaces, e.g. `0-2` (for EU) or `0-71` (for US).
 - **Set RX1 DR offset** defines the offset between the uplink and the RX1 slot
   downlink data rates

See the [Device Administration](Devices.md) guide for more details on the
server administration.


## Node Configuration

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
