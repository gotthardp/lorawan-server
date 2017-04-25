# Backend Administration

To use this functionality you have to set Device/Node Application to *backend*
and enter some *Group* name for this device/node.

## Handlers

To create a new handler you need to set:
 * *Group* name
 * *Format* to either *Raw* content or *JSON*
 * *Parse Uplink* and *Build Downlink* functions (for the JSON format only)
 * *Connector* identifier (see the next chapter)

The *Parse Uplink* is an Erlang function that converts a binary to a list that
gets JSON encoded into the `fields` attribute.

For example:

```erlang
fun(_Port, <<LED, Press:16, Temp:16, AltBar:16, Batt, Lat:24, Lon:24, AltGps:16>>) ->
  [{led, LED}, {pressure, Press}, {temp, Temp/100}, {alt_bar, AltBar}, {batt, Batt}]
end.
```

It shall be a
[Fun Expression](http://erlang.org/doc/reference_manual/expressions.html#funs)
with two parameters, which matches the
[binary data](http://erlang.org/doc/programming_examples/bit_syntax.html)
and returns an
[Erlang representation of JSON](https://github.com/talentdeficit/jsx#json---erlang-mapping).

*Build Downlink* works in the opposite direction. It takes whatever is in the
"fields" attribute and converts that into a binary. If you send `{"fields":1}`,
you can have a function like this:

```erlang
fun(_Port, LED) ->
  <<LED>>
end.
```

It shall be a
[Fun Expression](http://erlang.org/doc/reference_manual/expressions.html#funs)
with two parameters, which gets an
[Erlang representation of JSON](https://github.com/talentdeficit/jsx#json---erlang-mapping)
and returns
[binary data](http://erlang.org/doc/programming_examples/bit_syntax.html).


## Connectors

To create a new connector you set:
 * *Connector Name*
 * *Enabled* flag that allows you to temporarily disable an existing connector.
 * *URI* defines the target host either as `mqtt://host:port` or `mqtts://host:port`
 * *Published Topic*, which is a server pattern for constructing the publication
   topic, e.g. `out/{devaddr}`.
 * *Subscribe*, which is a topic to be subscribed, e.g. `in/#`. It may include
   broker specific wilcards.
 * *Consumed Topic*, which is is a server pattern for parsing topics of consumed
   messages, e.g. `in/{devaddr}`.

On the Authentication tab:
 * *Client ID* is the MQTT parameter
 * *Auth* identifies the type of authentication:
   * *Username+Password* for common servers
   * *Shared Access Signature* for Microsoft servers
 * *Name* and *Password/Key* for plain authentication
 * *User Certificate* and *Private Key* if SSL authentication is needed

To include node-specific attributes the topics may include following patterns:
 * `{devaddr}` that matches the DevAddr of the node
 * `{group}` that matches the node group

Please read the [Integration Guide](Integration.md) for detailed information on
how to connect to a generic MQTT server like RabbitMQ, AWS IoT, Azure IoT Hub or
Adafruit IO.
