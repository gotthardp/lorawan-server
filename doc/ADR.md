# Adaptive Data Rate (ADR)

Transmissions (uplinks) of each node can use different transmission power, data
rate and channels (frequencies). Depending on the *Set ADR* parameter this can
be manually defined by the administrator or automatically determined by the server.

Note this functionality must be also enabled in the device, which is indicated
by the *Use ADR* parameter.

## Automatic ADR

The server calculates an average RSSI and SNR for the last 20 frames received.
 * The *Data Rate* (Spreading Factor) is defined so that the LoRa demodulator SNR
   for the target *Data Rate* matches the observed average SNR.
 * When the highest *Data Rate* is reached, the *Power* is decreased, so the
   RSSI stays above -110 dbB, which is an expected sensitivity for the highest rate.


## Manual ADR

### Device Configuration

For each OTAA device you can define which ADR parameters shall be requested when
the device joins the network:
 - **Set ADR** can be used to request or disable ADR for this device;
 - **Set Power** defines the power (in dBm) to be requested;
 - **Set Data Rate** defines the data rate;
 - **Set Channels** defines the set of channels to be used. The channels are given
   as a comma-separated list of interfaces, e.g. `0-2` (for EU) or `0-71` (for US).

See the [Administration Guide](Administration.md) for more details on the
server administration.

![alt tag](https://raw.githubusercontent.com/gotthardp/lorawan-server/master/doc/images/admin-device.png)

### Link Configuration

For each connected node (link) you can define ADR paramaters to be requested by
the server. Initally, this is the Device Configuration.

The server administration also displays the node settings:
 - **Use ADR** indicates whether the node can do ADR;
 - **Use Power** indicates the last accepted TX power setting (in dBm);
 - **Use Data Rate** indicates the data rate;
 - **Use Channels** indicates the set of channels to be used.

The ADR request to change these settings is sent to the device only when both
**Set ADR** and **Use ADR** are `ON`, when all ADR settings are defined (not null)
and when some ADR parameter differs from the last accepted setting.

If the device does not support (or allow) some of the requested settings, the
entire request will fail. When a request fails, the requested ADR parameter(s) that
caused the failure will be cleared (set to null) and no other parameter will become
effective.

![alt tag](https://raw.githubusercontent.com/gotthardp/lorawan-server/master/doc/images/admin-link-status.png)
