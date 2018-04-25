# Connectors

Connectors define transport of data fields to/from external servers. Each
connector is linked with one Handler and specifies:
 * Communication protocol
 * Target endpoint, i.e server address and message topics
 * Encoding of the data fields

## Administration

To create a new connector you set:
 - **Connector Name**
 - **Application** that references a specific backend *Handler*
 - **Format** of the message payload, which can be:
   - **JSON** to encode data fields as [JSON](http://www.json.org) structures like `{"NameOne":ValueOne, "NameTwo":ValueTwo}`.
   - **Raw Data** to send just the binary content of the *data* field, without ant port numbers nor flags.
   - **Web Form** to encode fields in a query strings like `NameOne=ValueOne&NameTwo=ValueTwo`.
 - **URI** defines the target host, which can be
   - `ws:` (with no host:port) for Web Sockets
   - `http://` for HTTP POST or `https://` for HTTP/SSL
   - `mqtt://` for MQTT or `mqtts://` for MQTT/SSL
   - `amqp://` for AMQP or `amqp://` for AMQP/SSL
 - **Publish Uplinks**, which is a server pattern for constructing the publication
   topic for uplink messages, e.g. `out/{devaddr}`. This can be used to include
   the actual DevEUI, DevAddr or other data field in the message topic.
 - **Publish Events**, which is a server pattern for constructing the publication
   topic for event messages.
 - **Subscribe**, which is a topic to be subscribed. It may include broker specific
   wilcards, e.g. `in/#`. The MQTT broker will then send messages with a matching
   topic to this connector.
 - **Received Topic**, which is a template for parsing the topic of received
   messages, e.g. `in/{devaddr}`. This can be used to obtain a DevEUI, DevAddr or
   a device group that shall receive a given downlink.
 - **Enabled** flag that allows you to temporarily disable an existing connector.
 - **Failed** flag indicates what has failed. The Event list includes more
   details.
   - **badarg** when some of connector parameters is bad
   - **network** when the destination server cannot be reached
   - **topic** when the target broker configuration is wrong

When a *Failed* flag is raised, the connector is inactive and no connection is
established. To re-establish the connection after an error occures, fix the
indicated root cause and then remove the flag to reactivate the connector.

On the Authentication tab:
 - **Client ID** is the MQTT parameter
 - **Auth** identifies the type of authentication:
   - **Username+Password** for common servers
   - **Shared Access Signature** for Microsoft servers
 - **Name** and **Password/Key** for plain authentication
 - **User Certificate** and **Private Key** if SSL authentication is needed

If the Connector is *Enabled* the server will automatically connect to the
backend server and subscribe this topic.

Please read the [Integration Guide](Integration.md) for detailed information on
how to connect to a specific IoT Platform like AWS IoT, IBM Watson IoT, MathWorks
ThingSpeak, Azure IoT Hub or Adafruit IO.

### Patterns

To include and extract node-specific attributes the Published and Received Topic
may include data field names in curly brackets, e.g. `{deveui}` or `{port}`
(see the *Uplink Fields* of Backend Handlers).

If you set *Published Topic* to `/device/{deveui}` and `#{deveui => "123456"}`,
then the topic `/device/123456` will be used for the particulat frame.

Similarly, if your *Received Topic* is `/device/{deveui}` and the backend
publishes a downlink to `/device/123456`, the field `#{deveui => "123456"}`
will be set and used for routing the downlink.

The data fields you use in your patterns must be included as *Uplink Fields*
of the respective Handler.

The MQTT connector allows the `{devaddr}` and `{appargs}` parameter also for the
*URI* and *Authentication* fields to create device-specific connections.

If you set *Client ID* to `{devaddr}` the server will create as many MQTT
connections as many devices you have, each using its `devaddr` as the ClientID.

The Received Topic may also include the `#` (hash), which matches zero or more
characters, including any '/' delimiters. It can be used to ignore the leading
or training characters in the topic.

### Certificates

You can generate a self-signed *User Certificate* (`cert.pem`) and corresponding
*Private Key* (`key.pem`) by:
```
openssl req -x509 -newkey rsa:4096 -keyout privkey.pem -out cert.pem -days 365
openssl rsa -in privkey.pem -out key.pem
```

## Web Sockets

The lorwan-server can act as a Web Socket server. Clients can connect, receive
uplinks and send downlinks.

To create a web socket connector you set:
 - **URI** to `ws:` (with no hostname)
 - **Publish Uplinks** to a URL pattern starting with a slash, e.g. '/ws/uplink/{devaddr}'
 - **Publish Events** to another URL pattern, e.g. '/ws/events/{devaddr}'

The patterns may contain uplink Fields of the corresponding [Handler](Handlers.md),
mainly `{deveui}`, `{devaddr}` or `{app}` corresponding to a group of devices with
the same application (Handler name).

The *Subscribe* and *Received Topic* fields are not used and can be left empty.

To connect to the WebSocket, then open URL to the path you defined, i.e.
`ws://server:8080/ws/uplink/<DevAddr>` or `ws://server:8080/ws/events/<DevAddr>`.

The URL is matched against the pattern to select the target device(s). For example,
`ws://127.0.0.1:8080/ws/events/11223344` connects to events from the DevAddr *11223344*.

Multiple parallel connections may be established to one URL.
When the device sends a frame, all connected clients will receive the application data.
Any of the clients may then send a response back. If multiple clients send data to
the device the frames will be enqueued and sent one by one. The enqueued *Downlinks*
can be viewed via the [Node Administration](Nodes.md) page.

### Keep-alive

By default, the WebSocket connection will be closed if the client (e.g. your web browser)
sends for 1 hour no data back to the server. This is to avoid stale connections.

To keep the connection open for a longer time:
 * You can adjust the `{websocket_timeout, 360000}` configuration parameter to a higher
   value (in milliseconds).
 * You can even set `{websocket_timeout, infinity}` to disable the session expiration.
 * Or the client (browser) needs to keep sending *ping* frames.

The **ping** frames may not be enabled by default. To enable *ping* frames in Firefox,
go to **about:config** and set **network.websocket.timeout.ping.request** to (for example)
120 (seconds).

### Demo page

Demo client is available at [`http://127.0.0.1:8080/admin/ws.html`](../priv/admin/ws.html).
Enter the desired URL path (e.g. `/ws/events/11223344`) and the desired format
(Raw or JSON) and then establish a WebSocket connection.
The page will display data received from the device and allow you to send data back.

In the *Raw* mode all information must be entered as a string of hexadecimal digits,
without any spaces.
Each byte is represented by exactly 2 digits. For example, "4849" represents ASCII string "01".

## HTTP/REST

The REST API works in both directions. The lorwan-server can send a HTTP PUT/POST
request upon receiving an uplink frame. It can also listen for incoming
HTTP PUT/POST requests and trigger downlink frames.

To create a HTTP connector you set:
 - **URI** to the target host either as `http://host:port` or `http://host:port. Do
   not append any path: use the *Publish Uplinks* or *Events* field instead.
 - **Publish Uplinks** to a URL pattern starting with a slash, e.g. '/uplink/{devaddr}'
 - **Publish Events** to another URL pattern, e.g. '/events/{devaddr}'
 - **Received Topic** is a template for parsing the topic of received downlink
   messages, e.g. `/in/{devaddr}`.

Make sure that all URL paths start with a slash ('/'). The *Received Topic* must
be different to all Web Socket *Publish* patterns.

Every uplink will trigger a HTTP POST to `http://host:port/uplink/{devaddr}`.

To send a downlink request you should make a HTTP PUT or POST to
`http://yourserver:8080/in/{devaddr}` and authenticate using the admin credentials.
For example:

```bash
curl -v --digest -uadmin:admin -H "Content-Type: application/json" -X POST http://localhost:8080/in/11223344 -d "{\"data\":\"ABCDEFG\"}"
```

## Generic MQTT Servers

The lorawan-server can acts as a MQTT client, publish uplink frames and subscribe
for downlink frames. It can connect to any standard MQTT server (message broker),
e.g. the
[RabbitMQ](https://www.rabbitmq.com/mqtt.html) or
[Mosquitto](https://mosquitto.org).

First of all, make sure you understand the
[terminology and principles of messaging](http://www.rabbitmq.com/tutorials/tutorial-one-php.html)
and that the MQTT protocol [is enabled](https://www.rabbitmq.com/mqtt.html)
in your broker.

Open the lorawan-server web-administration and create a Backend Connector:
 - **URI** defines the target host either as `mqtt://host:port` or `mqtts://host:port`
 - **Publish Uplinks** is a pattern for constructing the message topic
   of uplinks, e.g. `out/{devaddr}`.
 - **Subscribe** is a downlink topic to be subscribed by the lorawan-server,
   e.g. `in/#`.
 - **Received Topic** is a template for parsing the topic of received downlink
   messages, e.g. `in/{devaddr}`.

On the Authentication tab:
 - **Auth** shall be set to *Username+Password*, even when the *Name* and
   *Password/Key* are empty.

Exclusively for MQTT, the *URI*, *Client ID*, *Name*, *Password/Key* and *Subscribe*
may also include patterns, but limited to `{devaddr}` and `{appargs}` only. This
can be used to create device-specific connections or subscriptions. Use with care:
creating too many connections may cause overload.

In order to consume the uplink messages sent by your devices you have to subscribe
at the message broker for the *Published Topic*, e.g. for `out/#` (or even just `#`) by:
```bash
mosquitto_sub -h 127.0.0.1 -p 1883 -t 'out/#' -u 'user' -P 'pass'
```

When using RabbitMQ, create a queue and then bind it to the `amq.topic` exchange
using the `out.#` (or `#`) binding key. Note that while MQTT uses slashes ("/") for
topic segment separators, RabbitMQ uses dots. RabbitMQ internally translates the two,
so for example the MQTT topic `cities/london` becomes in RabbitMQ `cities.london`.

To send a downlink message to one of your devices do e.g.
```bash
mosquitto_pub -h 127.0.0.1 -p 1883 -t 'in/00112233' -m '{"data":"00"}' -u 'user' -P 'pass'
```


## MongoDB

The lorawan-server can store the received uplinks directly to a
[MongoDB](https://www.mongodb.com).

Open the lorawan-server web-administration and create a Backend Connector:
 - **Format** is ignored, but should be set to *JSON*.
 - **URI** defines the target host `mongodb://host:port` or `mongodb://host1:port,host2:port`
   to list replica pairs/sets.
 - **Publish Uplinks** and **Publish Events** must be in the format *Database*/*Collection*.
   The *Database* is optional; if not provided, `local` is used by default.

Authentication is not supported.
