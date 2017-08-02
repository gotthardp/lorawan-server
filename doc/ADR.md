# Adaptive Data Rate (ADR)

Transmissions (uplinks) of each node can use different transmission power, data
rate and channels (frequencies). Depending on the *Set ADR* parameter this can
be manually defined by the administrator or automatically determined by the server.

Note this functionality must be also enabled in the device, which is indicated
by the *Use ADR* parameter.

![alt tag](https://raw.githubusercontent.com/gotthardp/lorawan-server/master/doc/images/admin-adr.png)


## Device Configuration

For each OTAA device you can define what ADR parameters shall be requested when
the device joins the network:
 - **Set ADR** can be used to request or disable ADR for this device;
 - **Set Power** defines the power (in dBm) to be requested;
 - **Set Data Rate** defines the data rate;
 - **Set Channels** defines the set of channels to be used. The channels are given
   as a comma-separated list of interfaces, e.g. `0-2` (for EU) or `0-71` (for US).

See the [Device Administration](Devices.md) guide for more details on the
server administration.


## Node Configuration

After a device joins the network the server requests the ADR parameters specified
in the Device ADR configuration explained above. These become the new requested
Node ADR parameters, regardless of any previous settings.

Note that the requested Node parameters are not affected by an ABP device reset.

The server administration also displays the currently used ADR settings:
 - **Used ADR** indicates whether the node can do ADR;
 - **Used Channels** indicates the set of channels to be used;
 - **RX** graph indicates the device Power (dBm), Data Rate and Frequency (MHz).

After a join or a reset of an ABP device the effective parameters are reverted to
their standard defaults.

The ADR request to change these settings is sent to the device when both **Set ADR**
and **Use ADR** are `ON` or `Manual`, when all requested ADR settings are defined
(not empty) and when some ADR parameter differs from the used (last accepted settings).

If the device does not support (or allow) some of the requested settings, the
entire request will fail. When a request fails, the requested ADR parameter(s) that
caused the failure will be cleared (set to empty) and no other parameter will become
effective.

### Manual ADR

For each connected node you can manually modify the requested ADR settings. The
ADR request will be sent to the node with the next downlink frame.

### Automatic ADR

You can also **Set ADR** to `ON` and let the server to automatically set the
requested ADR parameters depending on the signal quality.

The server calculates an average RSSI and SNR for the last 20 frames received.
 * The *Data Rate* (Spreading Factor) is defined so that the LoRa demodulator SNR
   for the target *Data Rate* matches the observed average uplink SNR.
 * When the highest *Data Rate* is reached, the *Power* is decreased, so the
   RSSI stays above -100 dbB, which is an expected sensitivity for the highest rate.
