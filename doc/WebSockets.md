# WebSocket Interface

To enable the WebSocket interface for a specific device you need to set the Device
or Link Application to `websocket`.

To connect to the WebSocket, open URL `ws://server:8080/ws/<Type>/<Name>/<Format>`. The
URL is used to select both the target device(s) as well as the desired format.

  Type / Name        | Behaviour
 --------------------|--------------------------------------------------------------------
  /devices/*123*/... | Connects to a device with a DevEUI=*123*.
  /links/*456*/...   | Connects to a link (active node) with a DevAddr=*456*.
  /groups/*ABC*/...  | Connects to all websocket devices whose **AppID** is set to *ABC*.

Multiple parallel connections may be established to one URL.
When the device sends a frame, all connected clients will receive the application data.
Any of the clients may then send a response back. If multiple clients send data to
the device the frames will be enqueued and sent one by one. The enqueued *Downlinks*
can be viewed via the [Administration interface](Administration.md).

  Format             | Behaviour
 --------------------|--------------------------------------------------------------------
  .../raw            | Transmits the application data only, no port numbers nor flags.
  .../json           | Transmits JSON structures.

For example, `ws://127.0.0.1:8080/ws/links/11223344/json` connects to the DevAddr *11223344*
using the JSON format.

The JSON structure the server sends to clients contains the following fields:

  Field       | Type        | Explanation
 -------------|-------------|-------------------------------------------------------------
  devaddr     | Hex String  | DevAddr of the link (active node).
  appargs     | Any         | Application arguments for this node.
  port        | Integer     | LoRaWAN port number.
  fcnt        | Integer     | Frame number
  data        | Hex String  | Raw application payload, encoded as a hexadecimal string.
  shall_reply | Boolean     | Whether the server has to send a downlink message.
  last_lost   | Boolean     | Whether the previous confirmed downlink was lost.
  rxq         | Record      | Reception quality

For example:
```json
    {"devaddr":"11223344", "port":2, "fcnt":58, "data":"0125D50B020BA23645F1A90BDDEE0004",
        "shall_reply":false, "last_lost":false,
        "rxq":{"lsnr":9.2,"rssi":-53,"tmst":3127868932,"codr":"4/5","datr":"SF12BW125","freq":868.3}}
```

The client may send back to the server a JSON structure with the following fields:

  Field       | Type        | Explanation
 -------------|-------------|-------------------------------------------------------------
  devaddr     | Hex String  | DevAddr of the link (active node).
  port        | Integer     | LoRaWAN port number. If not specified for Class A, the port number of last uplink will be used. Mandatory for Class C.
  data        | Hex String  | Raw application payload, encoded as a hexadecimal string.
  confirmed   | Boolean     | Whether the message shall be confirmed (false by default).
  pending     | Boolean     | Whether the application has more to send (false by default).
  time        | ISO 8601    | Requested downlink time or `immediately` (for class C devices only).

For example:
```json
    {"devaddr":"11223344", "data":"0026BF08BD03CD35000000000000FFFF", "confirmed":true}
    {"data":"00", "port":2, "time":"2017-03-04T21:05:30.2000"}
    {"data":"00", "port":2, "time":"immediately"}
```


## Keep-alive

By default, the WebSocket connection will be closed if the client sends no data for 1 hour.
This is to avoid stale connections.

To keep the connection open for a longer time:
 * You can adjust the `{websocket_timeout, 360000}` configuration parameter to a higher
   value (in milliseconds), or even to `infinity`.
 * Or the client (browser) needs to keep sending **ping** frames.

The **ping** frames may not be enabled by default. To enable **ping** frames in Firefox,
go to **about:config** and set **network.websocket.timeout.ping.request** to (for example)
120 (seconds).

## Demo page

Demo client is available at [`admin/ws.html`](../priv/admin/ws.html). Select the
target device or a group (DevEUI, DevAddr or AppID) and the desired format (Raw or JSON)
and establish a WebSocket connection.
The page will display data received from the device and allow you to send data back.

In the **Raw** mode all information must be entered as a string of hexadecimal digits,
without any spaces.
Each byte is represented by exactly 2 digits. For example, "4849" represents ASCII string "01".

In the **JSON** mode the JSON structure described above is used for both uplinks
and downlinks.
