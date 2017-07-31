# WebSocket Interface

To enable the WebSocket interface for a specific device you need to set the Device
or Node **Application** to `websocket`. You may also set the **Group** name as
explained below.

To connect to the WebSocket, open URL `ws://server:8080/ws/<Type>/<Name>/<Format>`. The
URL is used to select both the target device(s) as well as the desired format.

  Type / Name        | Behaviour
 --------------------|--------------------------------------------------------------------
  /devices/*123*/... | Connects to a device with a DevEUI=*123*.
  /nodes/*456*/...   | Connects to an active node with a DevAddr=*456*.
  /groups/*ABC*/...  | Connects to all websocket devices whose **Group** name is set to *ABC*.

Multiple parallel connections may be established to one URL.
When the device sends a frame, all connected clients will receive the application data.
Any of the clients may then send a response back. If multiple clients send data to
the device the frames will be enqueued and sent one by one. The enqueued *Downlinks*
can be viewed via the [Node Administration](Nodes.md) page.

  Format             | Behaviour
 --------------------|--------------------------------------------------------------------
  .../raw            | Transmits the raw application data only, no port numbers nor flags.
  .../json           | Transmits JSON structures described in the [JSON Payload](JSON.md) documentation.

For example, `ws://127.0.0.1:8080/ws/nodes/11223344/json` connects to the DevAddr *11223344*
using the JSON format.

The [Backend Handlers](Backends.md) are applicable also to WebSockets. If a JSON handler
is defined for the Device / Node *Group* the *Parse Uplink* and *Build Downlink* functions
are invoked to handle the "fields" attribute.


## Keep-alive

By default, the WebSocket connection will be closed if the client (e.g. your web browser)
sends for 1 hour no data back to the server. This is to avoid stale connections.

To keep the connection open for a longer time:
 * You can adjust the `{websocket_timeout, 360000}` configuration parameter to a higher
   value (in milliseconds).
 * You can even set `{websocket_timeout, infinity}` to disable the session expiration.
 * Or the client (browser) needs to keep sending **ping** frames.

The **ping** frames may not be enabled by default. To enable **ping** frames in Firefox,
go to **about:config** and set **network.websocket.timeout.ping.request** to (for example)
120 (seconds).


## Demo page

Demo client is available at [`http://127.0.0.1:8080/admin/ws.html`](../priv/admin/ws.html).
Select the target device or a group (DevEUI, DevAddr or Group) and the desired format
(Raw or JSON) and establish a WebSocket connection.
The page will display data received from the device and allow you to send data back.

In the **Raw** mode all information must be entered as a string of hexadecimal digits,
without any spaces.
Each byte is represented by exactly 2 digits. For example, "4849" represents ASCII string "01".

In the **JSON** mode the JSON structure described above is used for both uplinks
and downlinks.
