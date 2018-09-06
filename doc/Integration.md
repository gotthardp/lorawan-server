# Integration Guide

This document desribes specific configuration required to integrate with the
following IoT cloud platforms:
 * [Amazon AWS IoT](https://aws.amazon.com/iot/)
 * [IBM Watson IoT Platform](https://www.ibm.com/cloud-computing/bluemix/internet-of-things)
 * [MathWorks ThingSpeak](https://thingspeak.com/)
 * [Microsoft Azure IoT Hub](https://azure.microsoft.com/en-us/services/iot-hub/)
 * [ThingsBoard Open-source IoT Platform](https://thingsboard.io)
 * [Adafruit IO](https://io.adafruit.com/)
 * [Orange Live Objects](https://liveobjects.orange-business.com)

Please refer to [Connectors](Connectors.md) guide for a generic description if you
need to integrate with another platform.

## AWS IoT

[Amazon Web Services (AWS)](https://aws.amazon.com/iot/) can be integrated via MQTT.
All Nodes can share the same connection to the server.

First, follow the AWS IoT guidelines to configure your IoT device:
 * Create and activiate a Security Certificate. Make sure you download the certificate
   (`xxx-certificate.pem.crt`) as well as the private key (`xxx-private.pem.key`).
 * Create a policy and link it to the certificate. For example, to allow
   all actions you can set:
   ```
   "Statement": [{
     "Action": "iot:*",
     "Resource": "*",
     "Effect": "Allow"
   }]
   ```
 * Create a device and link it to the same certificate.

Then, open the lorawan-server web-administration and create an Backend Connector:
 - **URI** is the AWS *Endpoint* with the `mqtts://` prefix
 - **Publish Uplinks** is a pattern for the publication topic, e.g. `out/{devaddr}`.
   Make sure you added `devaddr` to your [Handler](Handlers.md) Fields.
 - **Subscribe** is a topic to be subscribed by the lorawan-server, e.g. `in/#`.
 - **Received Topic** is a template for parsing the topic of received messages, e.g. `in/{devaddr}`.

On the Authentication tab:
 - **Client ID** is the AWS *Account Id*
 - **Auth** shall be set to *Username+Password*
 - **User Certificate** is the `xxx-certificate.pem.crt` file you downloaded
 - **Private Key** is the `xxx-private.pem.key` file

Instead of creating a Security Certificate and then attaching it to the Thing
you can alternatively create a Thing Security Certificate. In such case the
*Client ID* shall be the Thing name.


## IBM Watson IoT Platform
[IBM Watson IoT Platform](https://www.ibm.com/cloud-computing/bluemix/internet-of-things)
can be integrated via MQTT. The lorawan-server can act as a Gateway acting on
behalf of multiple devices.

First, follow the IBM Bluemix documentation to configure the IoT Gateway:
 * Create a *device type* for your devices, e.g. "loramote"
 * Create a *gateway type* for the lorawan-server, e.g. "loraserver"
 * Create one gateway of the *gateway type* you just created using an arbitrary
   *Device ID*. After you click *Add* **don't close** the web-page displaying the
   auto-generated *Authentication Token*.
 * Do not create any devices; these will be created automatically once they send
   some data.

Then, open the lorawan-server web-administration and create an Backend Connector:
 - **URI** shall be `mqtt://orgid.messaging.internetofthings.ibmcloud.com`, where
   orgid is your *Organization ID* displayed on the web-page you didn't close.
 - **Publish Uplinks** is a pattern for the publication topic,
   e.g. `iot-2/type/loramote/id/{deveui}/evt/status/fmt/json`, where loramote is
   the *device type* you created.
 - **Subscribe** is a topic to be subscribed by the lorawan-server,
   e.g. `iot-2/type/loramote/id/+/cmd/+/fmt/+`.
 - **Received Topic** is a template for parsing the topic of received messages,
   e.g. `iot-2/type/loramote/id/{deveui}/cmd/status/fmt/json`.

On the Authentication tab:
 - **Client ID** shall be `g:orgid:loraserver:test`, where orgid is the *Organization ID*,
   loraserver is *gateway type* you created and test is the gateway *Device ID*.
 - **Auth** shall be set to *Username+Password*
 - **Name** shall always be `use-token-auth`
 - **Password/Key** is the gateway *Authentication Token* displayed on the page you
   didn't close.


## MathWorks ThingSpeak

[MathWorks ThingSpeak](https://thingspeak.com/) can be integrated via MQTT. It
supports only publishing to channels using MQTT, subscriptions are not supported
by ThingSpeak.

First, follow the ThingSpeak guidelines and create a New Channel:
 * Set a channel *Name*;
 * Define one or more *Field* labels;
 * Once you *Save Channel*, display the *Write API Key*.

Open the lorawan-server web-administration and create an Backend Connector:
 - **URI** shall be either `mqtt://mqtt.thingspeak.com` or `mqtts://mqtt.thingspeak.com`
 - **Publish Uplinks** shall be `channels/<channelID>/publish/<apikey>`, where
   * `<channelID>` is the numeric *Channel ID*
   * `<apikey>` is the *Write API Key*
 - **Subscribe** and **Received Topic** shall be left empty.

Then, create a new Handler:
 - **Group** shall correspond to a Group attribute of some Nodes.
 - **Format** shall be **Web Form**.
 - **Parse Uplink** shall include a data parsing function. See the [Backends](Backends.md)
   guide for more information, but make sure the values are named "field1", "field2" etc.
 - **Connector** shall link to the Backend Connector you just created.

For example, a *Parse Uplink* for the Semtech LoRaMote could be:
```erlang
fun(Vars, <<LED, Press:16, Temp:16, AltBar:16, Batt, Lat:24, Lon:24, AltGps:16>>) ->
  #{field1 => LED, field2 => Press, field3 => Temp/100, field4 => AltBar, field5 => Batt}
end.
```

## Microsoft Azure IoT Hub

[Microsoft Azure IoT Hub](https://azure.microsoft.com/en-us/services/iot-hub/)
can be integrated via MQTT. Azure uses per-device credentials so the Connector
will create one connection for each Node (each with its own credentials).

First, follow the Azure IoT Hub guidelines to configure your IoT devices:
 * Create a new Device:
   - **Device ID** shall be set to Device *DevEUI* or Node *DevAddr.
   - **Authentication Type** shall be *Symmetric Key*.
   - **Auto Generate Keys** shall be checked.
 * It is recommended to create also a *Shared access policy*, allowing
   *Device connect*.
 * Instead creating the access policy you may copy-paste the auto-generated device
   *Primary key* (encoded using Base64) to *App Arguments* in the Node config.

Then, open the lorawan-server web-administration and create an Backend Connector:
 - **URI** is the IoT Hub *Hostname* with the `mqtts://` prefix
 - **Publish Uplinks** shall be `devices/{devaddr}/messages/events/`.
   The trailing slash is mandatory.
 - **Subscribe** shall be `devices/{devaddr}/messages/devicebound/#`.
 - **Received Topic** shall be the same as *Subscribe*.

On the Authentication tab:
 - **Client ID** is the `{devaddr}`
 - **Auth** shall be set to *Shared Access Signature*
 - When authenticating using a *Shared access policy*:
   - **Name** is the *Access policy name* you created above
   - **Password/Key** is the access policy *Primary key* (encoded using Base64)
 - When authenticating using the device key:
   - **Name** shall be empty
   - **Password/Key** shall be `{appargs}`. Make sure you added `appargs` to your
     [Handler](Handlers.md) Fields.


## ThingsBoard Open-source IoT Platform

The [ThingsBoard](https://thingsboard.io) can be integrated via MQTT. The platform
uses per-device credentials so the Connector will create one connection for each Node.

First, follow the ThingsBoard documentation to configure your devices
 * Add new device:
   - **Name** shall be set to Device *DevEUI* or Node *DevAddr.
 * When created, enter the *Manage Credentials* tab and copy-paste *Access token*
   to *App Arguments* in the Node config.

Then, open the lorawan-server web-administration and create an Backend Connector:
 - **URI** shall be `mqtt://demo.thingsboard.io` or URL of your local ThingsBoard.
 - **Publish Uplinks** shall be `v1/devices/me/telemetry`.
 - **Publish Events** can be left empty, or set to `v1/devices/me/attributes`.

On the Authentication tab:
 - **Auth** shall be set to **Username+Password**
 - **Name** shall be `{appargs}`. Make sure you added `appargs` to your
   [Handler](Handlers.md) Fields.
 - Other fields are left empty.

Make sure that your Handler *Parse Uplink* and *Parse Event* produce a flat
list of string, boolean and numeric fields only.


## Adafruit IO

[Adafruit IO](https://io.adafruit.com/) supports MQTT and MQTT/SSL. Before doing
the integration, make sure you consult the following Adafruit articles:

 * [Adafruit IO](https://learn.adafruit.com/adafruit-io)
 * [Adafruit IO Basics: Feeds](https://learn.adafruit.com/adafruit-io-basics-feeds)
 * [Adafruit IO Basics: Dashboards](https://learn.adafruit.com/adafruit-io-basics-dashboards)
 * [MQTT, AdafruitIO & You!](https://learn.adafruit.com/mqtt-adafruit-io-and-you)

Once your Adafruit account, dashboards and feeds are set up, go to the
lorawan-server web-administration and create a Backends->Connector:
 - **URI** - `mqtt://io.adafruit.com` or `mqtts://io.adafruit.com`.
 - **Publish Uplinks** - Name of the topic you will be publishing to in the form
   `YourUserName/feeds/YourFeed`.
 - **Subscribe** - Name of the topic you want to receive data from in the form
   `YourUserName/feeds/YourFeed`.

On the Authentication tab:
 - **Client ID** - can be anything, but try to keep it unique among connectors.
 - **Auth** should be set to *Username+Password*:
   - **Name** - Your Adafruit account name (see the last article in the list above).
   - **Password/Key** - Your Adafruit Key, NOT your account password (see above).

Adafruit supports only a single value feeds, so it is a _limitation_, if your mote sends multiple values in one packet. Also, Adafruit
IO expects the payload of the MQTT message to be either a simple string value, like "17" or "string", or a proper formatted JSON of the form { "value": data } (ex. {"value": 23.5}, {"value": "sometext"}, etc). It should be noted, that if the source of data, especially
string data, is not really trustworthy or predictable it is safer to wrap the message into JSON packing, then to rely on the fact that
normally the messages are not JSON-like.

To achieve that you need to create a Handler with the `Format` field set to `Raw Data`, `Connector` field set to your Connector and `Parse Uplink`
function with the code to parse your sensor data and create a simple string or a JSON string expected by Adafruit.

This example function expects that the sensor is sending 16 bits of humidity, 16 bits of temperature (in tenths of a degree) and 1 byte checksum. It uses only temperature, because, as stated above, Adafruit feeds are single value. Temperature sign is encoded in the 16th bit.

```erlang
fun(_Port, <<_Humid:16, Temp0:16, _Csum>>) -> % Point 1
TSign = Temp0 band 16#8000, % Point 2
TVal = Temp0 band 16#7FFF, % Point 3
case TSign of % Point 4
  0 -> Temp = TVal / 10;
  _ -> Temp = -(TVal / 10)
end,
[H|_] = io_lib:format("~w", [Temp]), % Point 5
 <<"{\"value\":", (list_to_binary(H))/bytes, "}">> % Point 6
end.
```

What this code does:
1. Parse the data packet from the sensor into three fields: Humid, Temp0 and Csum. 16-bit, big-endian. Humid and Csum are not used,
so their names are prefixed with _ sign.
2. Get the sign indication (+/-) from the temperature value.
3. Strip off the sign indication from the temperature value.
4. According to the sign indication, get the temperature value in degrees instead of their tenths.
5. io_lib:format() function converts a given value into text representation, but returns a list. So, as we have only one value, we
take the first element of its result and drop everything else, resulting our value being a proper stringlist (H).
6. Build the resulting binary value to be sent to Adafruit IO: `{"value": temperature}`.

After all of this is ready, you need to select this Handler as a *Group* on your
*Devices* or *Nodes* configuration page. And **don't forget** to set your *Application*
field to **backend** there.


## Orange Live Objects

Define a new Handler:

Set **Uplink Fields** to `deveui` and `datetime`.

The **Parse Uplink** shall be as follows:
```erlang
fun(#{deveui := DevEui, datetime := DateTime}=Fields, Data) ->
  [[
    #{metadata => #source => {<<"urn:lora:", (lorawan_utils:binary_to_hex(DevEui))/binary>>},
      model => <<"your_model">>,
      streamId => <<"urn:lora:", (lorawan_utils:binary_to_hex(DevEui))/binary, "!uplink">>,
      tags => [],
      timestamp => DateTime,
      value => Fields#{payload => lorawan_utils:binary_to_hex(Data)}
    }
  ]]
end.
```

Then, define a new Connector:
 - **URI** set to `https://liveobjects.orange-business.com`
 - **Publish Uplinks** set to `/api/v0/data/bulk`
 - **Authetification** tab shall define
   - **Auth** Header+Token
   - **Name** `X-API-Key`
   - **Password/Key** shall be set to your key
