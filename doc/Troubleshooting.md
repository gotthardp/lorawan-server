# Troubleshooting Instructions

Overview of recent errors and warnings is provided in the server [Event List](Events.md).
Details can be found in the server logs.

Depending on your installation the server logs are stored in `/var/log/lorawan-server`,
`/var/lib/lorawan-server/log` or just `lorawan-server/log`. By default three log
files are provided: `debug.log`, `error.log` and `crash.log`. The log messages
contain date and time, severity (debug, info, notice, warning, error), process ID
and a description.

For example:
```
2017-03-05 02:14:57.148 [info] <0.361.0> Gateway <<1,39,235,255,255,176,23,196>> at {192,168,0,1}:47002
```

Binary identifiers are often shown in the Erlang binary notation, enclosed in `<< >>`. This
is simply a list of bytes in decimal notation.
For example, `<<1,39,235,255,255,176,23,196>>` means `01-27-EB-FF-FF-B0-17-C4`.

By default no messages are shown on the console. To see the debug messages you need to
open the server configuration in `lorawan-server/releases/<VERSION>/sys.config`,
uncomment (remove the `%` character) from the `{lager_console_backend, debug}` line
and restart the server.

## Common Errors

### No message in log file

The log shall always contain at least an [info] record `Gateway <<MAC>> at {IP}:PORT`.
If this is not listed, the gateway is down or the network connectivity is broken.

This may be due to:
 * Gateway misconfiguration. Check the gateway configuration includes a correct
   server IP and port (by default 1680).
 * Firewall misconfiguration. Check the server firewall does not block the port 1680.
   See [Installation Instructions](Installation.md) for configuration guidelines.

### No downlink frames delivered

This may be because:
 * Gateway configuration error. Verify you set correctly the *TX Chain* in your
   [Infrastructure](Infrastructure.md) configuration.
 * Gateway error. Check for any error in your gateway log file.
 * The device and gateway are using inconsistent private/public network mode setting.
 * The device did not listen on the channel (frequency) used by the server. Verify
   your device correctly listens in the right RX1/RX2 window. The RX2 frequencies
   and data rates are provided in the
   [regions](https://github.com/gotthardp/lorawan-server/blob/master/src/lorawan_server.app.src#L28)
   config parameter.

Some devices like the Arduino LMIC may have problems receiving downlinks in the RX1
window. To bypass this you can set the *TX Window* of your [Device](Devices.md)
or [Node](Nodes.md) to `RX2'.

### Join request received, but no further uplink frames

This is usually because the downlink frame including the Join response was not sent or
received by the device. See *No downlink frames sent* above.

### unknown_mac

The gateway *MAC* is not correctly configured.
Open the [Web Administration](Administration.md), go to the *Infrastructure-Gateways*
list and check the gateway *MAC* is correctly listed.

### unknown_deveui

The over-the-air activated (OTAA) device *DevEUI* is not correctly configured.
Open the [Web Administration](Administration.md), go to the *Devices* list
and check the device EUI is correctly listed.

### unknown_devaddr

The device *DevAddr* activated-by-personalization (ABP) is not correctly configured.
Open the [Web Administration](Administration.md), go to the *Nodes* list
and check the device address is correctly listed.

If you see strange *DevAddr* numbers from devices that you don't know, there
may be a second network near you. You can add devices from this network into
the list of [Ignored Nodes](Infrastructure.md).

### bad_mic

The Message Integrity Check (MIC) of a received frame has failed.
 * If this appeared only once this was due to a transmission error.
 * If this happens periodically with the same ID, the device is not correctly
   configured.
   * When this is related to a 8-bytes long *DevEUI*, open the
     [Web Administration](Administration.md),
     go to the *Devices* list and check the *AppKey*.
   * When this is related to a 4-bytes long *DevAddr*, go to the *Nodes* list
     and check the *NwkSKey*.

### fcnt_gap_too_large

The device sent an unexpected frame counter *FCnt*. This may be because:
 * The device is activated-by-personalization (ABP) and it did reset recently.
 * The device was unreachable for a very long time.

If this is an exceptional case, go to the *Nodes* list and manually update the
*FCnt Up* to the *FCnt* number.

To allow ABP devices to freely reset set the *FCnt Check* to *Reset on zero*,
but please note this weakens LoRaWAN security a bit.
It is recommended to use over-the-air-activation (OTAA) instead.

### repeated_reset

No frames were received since last OTAA join or last ABP reset. This is just a
warning. It may be because:
 * No downlink frames delivered (see above)
 * Faulty device is periodically re-starting

### not_semtech_mote

The server is configured to use the *semtech-mote* application, but your device
sent some other data. Make sure your firmware uses the
[supported format](https://github.com/Lora-net/LoRaMac-node/blob/master/src/apps/LoRaMac/classA/LoRaMote/main.c#L207).

### downlink_missed

Confirmed downlink was sent, but the device indicated it did not received it.
Depending on the application logic the downlink may be retransmitted.

### downlink_expired

Confirmed downlink was superseded before it could be transmitted to the device.
See the **D/L Expires** setting of the corresponding [Handler](Handlers.md).

### prerequisite_failed

This is reported when the lorawan-server is started with older Erlang/OTP. At
least 19.0 (or later) is required.

### connector_disabled

The server is configured to use a connector, which is disabled. Set the
*Enabled* flag of your [Backend](Backends.md) connector configuration.

### Unknown downlink target

This is reported when a [Backend](Backends.md) connector doesn't know to what
device a downlink shall be sent. You have two options:
 * Define a "devaddr" or "deveui" field in the JSON structure, or
 * Define the "Received Topic" as a template "in/{devaddr}", which causes the
   server to parse the topic and extract the DevAddr from there.

### Unknown element in JSON received from the Lorank8 gateway

The problem is the Lorank8 proprietary message format. In your gateway config
you likely have `stat_format` set to `idee_concise` or `idee_verbose`. You need
to change `stat_format` to `semtech` to get this working.

### Browser reports NS_ERROR_NET_INADEQUATE_SECURITY

You are using old version of Erlang that does not support the newest ciphers
required by HTTP/2. Erlang 20 is recommended for TLS/SSL.

As a quick fix, in Mozilla you can disable the checks in `about:config`, by
setting `network.http.spdy.enforce-tls-profile` to `false`.

### Lost admin password

If you forgot your admin password
 * connect to the cluster via remote shell by `erl -sname test -remsh lorawan@<hostname>`
 * delete the `user` database by `mnesia:delete_table(user).`
 * restart the server to recreate the `user` database with the default admin password


## Alarms

Server alarms are indicated in the web-admin Dashboard.

### system_memory_high_watermark

This indicates that the system has less than 20% of free memory.

### disk_almost_full

This indicates there is less than 20% of free disk space.


## Issue Reporting

If this guideline didn't help you to solve your problem, feel free to report a
new issue. For the better understanding of the issue you are encouraged to provide:
 *  Distribution type: binary or built from source.
 *  Hardware type: PC Server, Just PC, MacBook, MacServer, Raspberry Pi1/2/3/x, Odroid, etc.
 *  OS type and version. Also, add the output of the `uname -a` command if you
    are running on a sort of a Unix/Linux/MacOS system.
 *  Your Erlang/OTP version. Can be obtained by running `erl` at the command line:
    please provide all the text before 1> line.
 *  If you are experiencing problems with the web-interface or seeing lots of
    http_error lines in the log , provide npm -v and node -v output too.

Get ready to provide your server's debug.log file and, in hard cases,
the [Wireshark](https://www.wireshark.org) network capture file.
