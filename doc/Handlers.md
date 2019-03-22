# Backend Handlers

Handlers define externally handled application, including:
 * Format of the uplink and downlink messages
 * Data fields forwared via the backend *Connectors*
 * Retransmission logic for confirmed downlinks

Each Handler may be linked with one or more backend *Connectors*, which handle
the communication towards the backend server.

The Handler will process every uplink frame and forward it to the backend. It
will also process every downlink request received from the backend.

In addition to uplink frames the backend can receive device related events:
 * when a device **joined**
 * when a confirmed frame was **delivered**
 * when a confirmed frame was **lost**
 * for a connection **test**


## Administration
![alt tag](https://raw.githubusercontent.com/gotthardp/lorawan-server/master/doc/images/admin-handler.png)

To create a new handler you need to set:
 - **Application** name
 - **Uplink Fields** that will be forwarded to the backend Connector
 - **Payload** format for automatic decoding
   - **ASCII Text**
   - [**Cayenne LPP**](https://github.com/myDevicesIoT/cayenne-docs/blob/master/docs/LORA.md)
   - [**CBOR**](https://tools.ietf.org/html/rfc7049)
   - **Custom Binary** decoded by the **Parse Uplink** function
 - **Parse Uplink** function to extract additional data fields from the uplink frame
 - **Event Fields** that will be forwarded to the backend Connector
 - **Parse Event** function to amend event data fields
 - **Build Downlink** function to create a downlink frame based on backend data fields
 - **D/L Expires** defines when the downlinks may be dropped.
   - **Never** means that:
     * All class A downlinks for a device will be queued and eventually delivered.
     * All confirmed downlinks will be retransmitted until acknowledged, even
       when a new downlink is sent.
   - **When Superseded** means that:
     * Only the most recent class A downlink will be scheduled for delivery.
       Superseded downlinks will be dropped.
     * Unacknowledged downlinks will be dropped when a new downlink (either
       class A or C) is sent.

**Test** button can be used to send a `test` event to all connections associated with this
handler.

**Connectors** related to this Handler are displayed for your convenience. The table
lists all backend connectors with the same *Application* name.


## Fields

### Uplink

Depending on the *Uplink Fields* settings the server sends to backend
applications the following fields:

  Field      | Type        | Meaning
 ------------|-------------|----------------------------------------------
  netid      | Hex String  | Network identifier (NetID).
  app        | String      | Application (Handler) name.
  devaddr    | Hex String  | DevAddr of the active node.
  deveui     | Hex String  | DevEUI of the device.
  appargs    | Any         | Application arguments for this node.
  desc       | String      | Custom description of this node.
  battery    | Integer     | Most recent battery level reported by the device.
  fcnt       | Integer     | Received frame sequence number.
  port       | Integer     | LoRaWAN port number.
  data       | Hex String  | Raw application payload, encoded as a hexadecimal string.
  datetime   | ISO 8601    | Timestamp using the server clock.
  freq       | Number      | RX central frequency in MHz (unsigned float, Hz precision).
  datr       | String      | LoRa datarate identifier (eg. "SF12BW500").
  codr       | String      | LoRa ECC coding rate identifier (usually "4/5").
  best_gw    | Object      | Gateway with the strongest reception.
  mac        | Hex String  | MAC address of the gateway with the strongest reception.
  lsnr       | Number      | LoRa uplink SNR ratio in dB (signed float, 0.1 dB precision) (same as rxq.lsnr for best_gw)
  rssi       | Number      | RSSI in dBm (signed integer, 1 dB precision) (same as rxq.rssi for best_gw)
  all_gw     | Object List | List of all gateways that received the frame.

The Gateway object included in *best_gw* and *all_gw* has the following fields:

  Field      | Type        | Explanation
 ------------|-------------|-------------------------------------------------------------
  mac        | Hex String  | MAC address of the gateway that received the frame.
  desc       | String      | Custom description of the gateway.
  rxq        | Object      | Indicators of the reception quality as indicated in the `rxpk` structure by the gateway (see Section 4 of the [packet_forwarder protocol](https://github.com/Lora-net/packet_forwarder/blob/master/PROTOCOL.TXT).
  rxq.lsnr   | Number      | LoRa uplink SNR ratio in dB (signed float, 0.1 dB precision)
  rxq.rssi   | Number      | RSSI in dBm (signed integer, 1 dB precision)
  rxq.tmst   | Number      | Internal timestamp of "RX finished" event (32b unsigned) used for response scheduling; it doesn't indicate any calendar date.
  gpsalt     | Number      | GPS Altitude.
  gpspos     | Object      | GPS Latitude and Longitude.

For example:
```json
    {"devaddr":"11223344", "port":2, "fcnt":58, "data":"0125D50B020BA23645F1A90BDDEE0004",
        "shall_reply":false, "last_lost":false,
        "rxq":{"lsnr":9.2,"rssi":-53,"tmst":3127868932,"codr":"4/5","datr":"SF12BW125","freq":868.3}}
```

### Downlink

To send a downlink you must define a target node (or a group of nodes) by using
*one of the following* fields either in the **Received Topic** template or in
the message body:

  Field       | Type        | Destination
 -------------|-------------|-------------------------------------------------------------
  app         | String      | All nodes for this application (Handler name).
  deveui      | Hex String  | (Commissioned) Device with this DevEUI.
  devaddr     | Hex String  | (Activated) Node with this DevAddr.

In addition to that you may specify the following optional fields:

  Field       | Type        | Explanation
 -------------|-------------|-------------------------------------------------------------
  desc        | String      | Custom description of the node.
  time        | ISO 8601    | Specifies requested downlink time or `immediately`. When specified, the downlink is considered as Class C.
  port        | Integer     | LoRaWAN port number in the range 1-223. Optional for Class A: if not specified, the uplink port number will be used. Mandatory for Class C.
  data        | Hex String  | Raw application payload, encoded as a hexadecimal string.
  confirmed   | Boolean     | Whether the message shall be confirmed (false by default).
  pending     | Boolean     | Whether the application has more to send (false by default).
  receipt     | Any         | If present, the delivery confirmation (**delivered** or **lost** event) will be sent for *confirmed* downlinks. The value of this field will be sent back in the *receipt* field of the event.

For example (class A):
```json
    {"devaddr":"11223344", "data":"0026BF08BD03CD35000000000000FFFF"}
    {"devaddr":"11223344", "data":"0026BF08BD03CD35000000000000FFFF", "confirmed":true, "receipt":"123XYZ"}
```
Or (class C):
```json
    {"data":"00", "port":2, "time":"2017-03-04T21:05:30.2000"}
    {"data":"00", "port":2, "time":"immediately"}
```
The `time` field must **not** be present if you want to send a Class A downlink.

### Events

Depending on the *Event Fields* settings the server sends to backend
applications the following fields:

  Field      | Type        | Meaning
 ------------|-------------|----------------------------------------------
  app        | String      | Application (Handler) name.
  event      | String      | Event name (joined, delivered, lost, test).
  devaddr    | Hex String  | DevAddr of the active node.
  deveui     | Hex String  | DevEUI of the device.
  appargs    | Any         | Application arguments for this node.
  datetime   | ISO 8601    | Timestamp using the server clock.
  receipt    | Any         | Custom data sent in the *receipt* field of the confirmed downlink request.


## Payload

The server can auto-parse some well-known data formats. In such case you don't
need to write own *Parse Uplink* function.

### ASCII Text

The payload will get stored into the `text` field as ASCII characters. This can
can be used only when the device directly sends human readable text.

### Cayenne Low Power Payload (LPP)

For each Data Channel *N* the server will create a `fieldN` with the parsed value.
See [Format Specification](https://github.com/myDevicesIoT/cayenne-docs/blob/master/docs/LORA.md#cayenne-low-power-payload).

For example:

<table style="width: 100%;">
<tbody>
<tr>
<td style="font-size: 15px; padding: 10px;"><b>Payload (Hex)</b></td>
<td style="font-size: 15px; padding: 10px;" colspan="2">03 67 01 10 05 67 00 FF</td>
</tr>
<tr>
<td style="font-size: 15px; padding: 10px;"><b>Data Channel</b></td>
<td style="font-size: 15px; padding: 10px;"><b>Type</b></td>
<td style="font-size: 15px; padding: 10px;"><b>Value</b></td>
</tr>
<tr>
<td>03 ⇒ 3</td>
<td>67 ⇒ Temperature</td>
<td>0110 = 272 ⇒ 27.2°C</td>
</tr>
<tr>
<td>05 ⇒ 5</td>
<td>67 ⇒ Temperature</td>
<td>00FF = 255 ⇒ 25.5°C</td>
</tr>
<tr>
<td style="font-size: 15px; padding: 10px;"><b>Fields</b></td>
<td style="font-size: 15px; padding: 10px;" colspan="2">#{<<"field3">> => 27.2, <<"field5">> => 25.5}</td>
</tr>
</tbody>
</table>

<table style="width: 100%;">
<tbody>
<tr>
<td style="font-size: 15px; padding: 10px;"><b>Payload (Hex)</b></td>
<td style="font-size: 15px; padding: 10px;" colspan="2">01 88 06 76 5f <i>f2 96 0a</i> <i>00 03 e8</i></td>
</tr>
<tr>
<td style="font-size: 15px; padding: 10px;"><b>Data Channel</b></td>
<td style="font-size: 15px; padding: 10px;"><b>Type</b></td>
<td style="font-size: 15px; padding: 10px;"><b>Value</b></td>
</tr>
<tr>
<td rowspan="3">01 ⇒ 1</td>
<td rowspan="3">88 ⇒ GPS</td>
<td>Latitude: 06765f ⇒ 42.3519</td>
</tr>
<tr>
<td><i>Longitude: F2960a ⇒ -87.9094</i></td>
</tr>
<tr>
<td><i>Altitude: 0003E8 ⇒ 10 meters</i></td>
</tr>
<tr>
<td style="font-size: 15px; padding: 10px;"><b>Fields</b></td>
<td style="font-size: 15px; padding: 10px;" colspan="2">#{<<"field1">> => #{lat => 42.3519, lon => -87.9094, alt => 10.0}}</td>
</tr>
</tbody>
</table>

### Custom Binary

To parse a custom format you need to write own *Parse Uplink* function.


## Parse Uplink

The *Parse Uplink* is an Erlang function that converts binary data to custom
data fields and can extend (or even amend) the *Uplink Fields*.

This function is optional. If not provided, only the *Uplink Fields* will be
sent to the Backend.

If provided, *Parse Uplink* shall be a
[Fun Expression](http://erlang.org/doc/reference_manual/expressions.html#funs)
with two parameters: *Fields* and a binary pattern. The function shall match the
[binary data](http://erlang.org/doc/programming_examples/bit_syntax.html)
and return a
[map expression](https://github.com/talentdeficit/jsx#json---erlang-mapping)
with the desired fields.

The selected **Uplink Fields** are provided in the `Fields` variable, which you
extend, for example:

```erlang
fun(Fields, <<LED, Press:16, Temp:16, AltBar:16, Batt, Lat:24, Lon:24, AltGps:16>>) ->
  Fields#{led => LED, pressure => Press, temp => Temp/100, alt_bar => AltBar, batt => Batt}
end.
```

Or even modify, for example:
```erlang
fun(#{fcnt := FCnt}, <<LED, Press:16, Temp:16, AltBar:16, Batt, Lat:24, Lon:24, AltGps:16>>) ->
  #{seq => FCnt, led => LED, pressure => Press, temp => Temp/100, alt_bar => AltBar, batt => Batt}
end.
```

To accept various frames in one function you can write alternative functions,
for example:
```erlang
fun (Fields, <<16#0402:16, Temp:16/signed>>) ->
        Fields#{temp => Temp};
    (Fields, <<16#0405:16, Level>>) ->
        Fields#{level => Level}
end.
```

To send multiple messages based on one frame (or even discard the frame and send
no message) the function may also return a list of map expressions, for example:

```erlang
fun(Fields, <<LED, Press:16, Temp:16, AltBar:16, Batt, Lat:24, Lon:24, AltGps:16>>) ->
  [
    Fields#{led => LED},
    Fields#{pressure => Press},
    Fields#{temp => Temp/100}
  ]
end.
```

This will generate 3 messages, each including the selected **Uplink Fields**
plus one data field.

To send one message with a list of expressions, send a list-in-list, for example:

```erlang
fun(Fields, <<LED, Press:16, Temp:16, AltBar:16, Batt, Lat:24, Lon:24, AltGps:16>>) ->
  [
    [
      Fields#{led => LED},
      Fields#{pressure => Press},
      Fields#{temp => Temp/100}
    ]
  ]
end.
```

This will generate one message containing a list of 3 items.

The `<<A, B, C>>` used to match the frame payload is a binary pattern, where A,
B, C are "variables" corresponding to the values encoded in the binary. Erlang
matches the incoming binary data against this pattern and fills the "variables"
with the values in the binary. Here are some examples:
 * `<<A>>` matches 1 value, 1 byte long.
 * `<<A, B>>` matches 2 values, each 1 byte long.
 * `<<A:16>>` matches 1 unsigned int value, 2 bytes long in big-endian
 * `<<A:16/little-signed-integer>>` matches 1 signed int value, 2 byes long in little-endian
 * `<<A:2/binary>>` matches an array of 2 bytes

To match a variable sized array of bytes, prefixed with a size byte, you can do:

```erlang
fun(Fields, <<Count, Data:Count/binary>>) ->
  Fields#{data => binary_to_list(Data)}
end.
```

The expression `#{name1 => A, name2 => B, name3 => C}` then creates (depending on
your [Connector](Connectors.md) settings) a JSON `{"name1":A, "name2":B, "name3":C}`,
or a Web-Form `name1=A&name2=B&name3=C`.

### Retained Messages

The field retain has a special meaning for the MQTT handler:

 * retain = false - create a non-retained message
 * retain = true - create a retained message
 * retain = delete - delete the retained message and create a non-retained
   message

The field retain is not sent in the message to the MQTT handler.

If the field retain is not present, a non-retained message is created.

## Parse Event

The *Parse Event* is an Erlang function that converts event name to custom
data fields and can extend (or even amend) the *Uplink Fields*.

Also this function is optional. If not provided, only the *Uplink Fields* will be
sent to the Backend.

To generate events like `{"devaddr":"00112233", "event":"joined"}` you can write:

```erlang
fun(Vars, Event) ->
  Vars#{event => Event}
end.
```

Alternatively, to generate an object like `{"joined":{"devaddr":"00112233"}}`
write:

```erlang
fun(Vars, Event) ->
  #{Event => Vars}
end.
```

Returning a list to send multiple event messages is not allowed.


## Build Downlink

*Build Downlink* works in the opposite direction. It takes the data fields and
constructs the binary payload.

This function is optional. If not provided, the downlink data will be taken
from the `data` field, e.g. when you send `{"devaddr":"11223344", "data":"01"}`.
The `data` field must be in hexadecimal notation.

If provided, *Build Downlink* shall be a
[Fun Expression](http://erlang.org/doc/reference_manual/expressions.html#funs)
with a single parameter, which gets an
[Erlang representation of JSON](https://github.com/talentdeficit/jsx#json---erlang-mapping)
and returns
[binary data](http://erlang.org/doc/programming_examples/bit_syntax.html).

JSON data is converted to a map. Fields taken from the MQTT topic are added to
the map. This map is passed as sole parameter to the downlink function. The
download function may either return a binary, a map, or a (possibly empty) list
of maps.
 - If a binary is returned, this binary is used to fill the `data` field which
   is sent to the device.
 - If a map is returned, this map replaces the original map.
   All relevant fields like `devaddr`, `deveui`, `port`, and `data` may be set
   in the downlink function.
 - If a list of maps is returned, the server triggers multiple downlink messages,
   one for each list item.
   Upon returning an empty list no downlink will be triggered.

For example, if you send `{"devaddr":"11223344", "led":1}`, you can have a function
like this to convert the custom field (`led`) to downlink data:

```erlang
fun(#{led := LED}) ->
  <<LED>>
end.
```

The `#{name1 := A, name2 := B, name3 := C}` matches the `fields` attribute containing
a JSON structure `{"name1":A, "name2":B, "name3":C}`. The order is not significant,
but all fields are mandatory.

The binary is then built using similar approach as the pattern matching
explained above. For example, `<<A, B, C>>` builds a binary of three 1-byte integers.

To build a variable sized array you can do:

```erlang
fun(#{data := Data}) ->
  <<(length(Data)), (list_to_binary(Data))/binary>>
end.
```

Here is a simple example just adding a `port` and a `data` field to the original
map:

```erlang
fun(Fields) ->
  Fields#{
    port => 43,
    data => << 16#88 >>
  }
end.
```
