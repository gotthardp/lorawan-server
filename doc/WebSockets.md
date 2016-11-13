# WebSocket Interface

To use the WebSocket interface you need to set the Device or Link Application to `websocket`.

Various protocols may be supported over WebSockets. Currently only one:

  URL             | Explanation
 -----------------|--------------------------------------------------------------------
  /ws/*123*/raw   | Connection to raw data from a device with DevEUI=*123*. Does not support port numbers.

Multiple parallel connections may be established to one device. If multiple clients
send data to a device the frames will be enqueued and sent one by one.

## Demo page

Demo client is available at [`admin/ws.html`](../priv/admin/ws.html). Enter DevEUI of a desired device and
establish a WebSocket connection. The page will display data received from the device
and allow you to send data back.

All information must be entered as a string of hexadecimal digits, without any spaces.
Each byte is represented by exactly 2 digits. For example, "4849" represents ASCII string "01".
