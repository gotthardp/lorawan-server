# JSON Payload

Both [WebSockets](WebSockets.md) as well as MQTT [Backends](Backends.md) can
use JSON structures as payload.
[JSON (JavaScript Object Notation)](http://www.json.org) is a lightweight
data-interchange format, which is easy to read and write both for humans as well
as for machines.

## Uplinks

The JSON structure the server sends to clients contains the following fields:

  Field       | Type        | Explanation
 -------------|-------------|-------------------------------------------------------------
  devaddr     | Hex String  | DevAddr of the active node.
  appargs     | Any         | Application arguments for this node.
  port        | Integer     | LoRaWAN port number.
  fcnt        | Integer     | Received frame sequence number.
  data        | Hex String  | Raw application payload, encoded as a hexadecimal string.
  fields      | Object      | Payload decoded by a corresponding [backend handler](Backends.md).
  shall_reply | Boolean     | Whether the server has to send a downlink message. By default `false`.
  last_lost   | Boolean     | Whether the previous confirmed downlink was lost. By default `false`.

Additional *Uplink Fields* may be enabled in the [Backend](Backends.md) configuration:

  Field       | Type        | Explanation
 -------------|-------------|-------------------------------------------------------------
  gateway     | Object      | Gateway that received the frame.
  gateway.mac | Hex String  | MAC address of the gateway.
  deveui      | Hex String  | DevEUI of the device.
  datetime    | ISO 8601    | Timestamp using the server clock.
  rxq         | Object      | Indicators of the reception quality.
  rxq.lsnr    | Number      | LoRa uplink SNR ratio in dB (signed float, 0.1 dB precision)
  rxq.rssi    | Number      | RSSI in dBm (signed integer, 1 dB precision)
  rxq.tmst    | Number      | Internal timestamp of "RX finished" event (32b unsigned)
  rxq.codr    | String      | LoRa ECC coding rate identifier
  rxq.datr    | String      | LoRa datarate identifier (eg. SF12BW500)
  rxq.freq    | Number      | RX central frequency in MHz (unsigned float, Hz precision)

For example:
```json
    {"devaddr":"11223344", "port":2, "fcnt":58, "data":"0125D50B020BA23645F1A90BDDEE0004",
        "shall_reply":false, "last_lost":false,
        "rxq":{"lsnr":9.2,"rssi":-53,"tmst":3127868932,"codr":"4/5","datr":"SF12BW125","freq":868.3}}
```

## Downlinks

The client may send back to the server a JSON structure with the following fields:

  Field       | Type        | Explanation
 -------------|-------------|-------------------------------------------------------------
  deveui      | Hex String  | DevEUI of the device.
  devaddr     | Hex String  | DevAddr of the active node.
  port        | Integer     | LoRaWAN port number. If not specified for Class A, the port number of last uplink will be used. Mandatory for Class C.
  data        | Hex String  | Raw application payload, encoded as a hexadecimal string.
  fields      | Object      | Payload to be encoded by a corresponding [backend handler](Backends.md).
  confirmed   | Boolean     | Whether the message shall be confirmed (false by default).
  pending     | Boolean     | Whether the application has more to send (false by default).
  time        | ISO 8601    | Requested downlink time or `immediately` (for class C devices only).

For example:
```json
    {"devaddr":"11223344", "data":"0026BF08BD03CD35000000000000FFFF", "confirmed":true}
```
Or (for class C devices only):
```json
    {"data":"00", "port":2, "time":"2017-03-04T21:05:30.2000"}
    {"data":"00", "port":2, "time":"immediately"}
```
