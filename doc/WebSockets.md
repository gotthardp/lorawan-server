# WebSocket Interface

To use the WebSocket interface you need to set the Device or Link Application to `websocket`.

Various protocols may be supported over WebSockets. Currently only one:

  URL             | Explanation
 -----------------|--------------------------------------------------------------------
  /ws/*123*/raw   | Connection to raw data from a device with DevAddr=*123* or DevEUI=*123*. Sends the application data only, no port numbers nor flags.

Multiple parallel connections may be established to one URL.
When the device sends a frame, all connected clients will receive the application data.
Any of the clients may then send a response back. If multiple clients send data to
the device the frames will be enqueued and sent one by one. The enqueued *Downlinks*
can be viewed via the [Administration interface](Administration.md).

## Demo page

Demo client is available at [`admin/ws.html`](../priv/admin/ws.html). Enter DevEUI of
a desired device and establish a WebSocket connection. The page will display data
received from the device and allow you to send data back.

All information must be entered as a string of hexadecimal digits, without any spaces.
Each byte is represented by exactly 2 digits. For example, "4849" represents ASCII string "01".
