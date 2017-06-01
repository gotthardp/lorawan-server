# Integration Guide

## Generic MQTT Server

You can integrate with generic MQTT server, e.g. the
[RabbitMQ](https://www.rabbitmq.com/mqtt.html) or
[Mosquitto](https://mosquitto.org).

Open the lorawan-server web-administration and create a Backend Connector:
 * *URI* defines the target host either as `mqtt://host:port` or `mqtts://host:port`
 * *Published Topic* is a pattern for constructing the message topic, e.g. `out/{devaddr}`.
 * *Subscribe* is a topic to be subscribed, e.g. `in/#`.
 * *Consumed Topic* is a pattern for parsing the message topic, e.g. `in/{devaddr}`.

On the Authentication tab:
 * *Auth* shall be set to *Username+Password*, even when the *Name* and
   *Password/Key* are empty.

To send a downlink message do e.g.
```bash
mosquitto_pub -h 127.0.0.1 -p 1883 -t 'in/00112233' -m '{"data":"00"}' -u 'user' -P 'pass'
```


## AWS IoT

Amazon Web Services (AWS) can be integrated via MQTT. All Nodes can share the same
connection to the server.

First, follow the AWS IoT guidelines to configure your IoT device:
 * Create and activiate a certificate. Make sure you download the certificate
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
 * *URI* is the AWS *Endpoint* with the `mqtts://` prefix
 * *Published Topic* is a pattern for the publication topic, e.g. `out/{devaddr}`.
 * *Subscribe* is a topic to be subscribed, e.g. `in/#`.
 * *Consumed Topic* is a pattern for the subscribed topic, e.g. `in/{devaddr}`.

On the Authentication tab:
 * *Client ID* is the AWS *Account Id*
 * *Auth* shall be set to *Username+Password*
 * *User Certificate* is the `xxx-certificate.pem.crt` file you downloaded
 * *Private Key* is the `xxx-private.pem.key` file


## IBM Watson IoT Platform

IBM Watson IoT Platform can be integrated via MQTT. The lorawan-server can act
as a Gateway acting on behalf of multiple devices.

First, follow the IBM Bluemix documentation to configure the IoT Gateway:
 * Create a *device type* for your devices, e.g. "loramote"
 * Create a *gateway type* for the lorawan-server, e.g. "loraserver"
 * Create one gateway of the *gateway type* you just created using an arbitrary
   *Device ID*. After you click **Add** don't close the web-page displaying the
   auto-generated *Authentication Token*.
 * Do not create any devices; these will be created automatically once they send
   some data.

Then, open the lorawan-server web-administration and create an Backend Connector:
 * *URI* shall be `mqtt://orgid.messaging.internetofthings.ibmcloud.com`, where
   orgid is your *Organization ID* displayed on the web-page you didn't close.
 * *Published Topic* is a pattern for the publication topic,
   e.g. `iot-2/type/loramote/id/{deveui}/evt/status/fmt/json`, where loramote is
   the *device type* you created.
 * *Subscribe* is a topic to be subscribed, e.g. `iot-2/type/loramote/id/+/cmd/+/fmt/+`.
 * *Consumed Topic* is a pattern for the subscribed topic,
   e.g. `iot-2/type/loramote/id/{deveui}/cmd/status/fmt/json`.

On the Authentication tab:
 * *Client ID* shall be `g:orgid:loraserver:test`, where orgid is the *Organization ID*,
   loraserver is *gateway type* you created and test is the gateway *Device ID*.
 * *Auth* shall be set to *Username+Password*
 * *Name* shall always be `use-token-auth`
 * *Password/Key* is the gateway *Authentication Token* displayed on the page you
   didn't close.


## Microsoft Azure IoT Hub

Microsoft Azure IoT Hub can be integrated via MQTT. Azure uses per-device credentials
so you cannot connect multiple devices (each with its own credentials) using the
same connection. A dedicated Connector is needed for each Node.

First, follow the Azure guidelines to configure your IoT device:
 * Create a new Device, use *Symmetric Key* authentication and let the system to
   *Auto Generate Keys*.
 * Optionally you may also define a *Shared access policy*.

Then, open the lorawan-server web-administration and create an Backend Connector:
 * *URI* is the IoT Hub *Hostname* with the `mqtts://` prefix
 * *Published Topic* shall be `devices/{devaddr}/messages/events/`.
   The trailing slash is mandatory.
 * *Subscribe* shall be `devices/xxxxxxxx/messages/devicebound/#`.
 * *Consumed Topic* shall be `devices/{devaddr}/messages/devicebound/#`.

On the Authentication tab:
 * *Client ID* is the *Device ID*
 * *Auth* shall be set to *Shared Access Signature*
 * When authenticating using the device key:
   * *Name* shall be empty
   * *Password/Key* is the device *Primary key* (encoded using Base64)
 * When authenticating using a *Shared access policy*:
   * *Name* is the *Access policy name*
   * *Password/Key* is the access policy *Primary key* (encoded using Base64)


## Adafruit IO

AdafruitIO supports MQTT and MQTT/SSL. Before doing the integration, make sure
you consult the following Adafruit articles:

 * [Adafruit IO](https://learn.adafruit.com/adafruit-io)
 * [Adafruit IO Basics: Feeds](https://learn.adafruit.com/adafruit-io-basics-feeds)
 * [Adafruit IO Basics: Dashboards](https://learn.adafruit.com/adafruit-io-basics-dashboards)
 * [MQTT, AdafruitIO & You!](https://learn.adafruit.com/mqtt-adafruit-io-and-you)

Once your Adafruit account, dashboards and feeds are set up, go to the
lorawan-server web-administration and create a Backends->Connector:
 * *URI* - `mqtt://io.adafruit.com` or `mqtts://io.adafruit.com`.
 * *Published Topic* - Name of the topic you will be publishing to in the form
   `YourUserName/feeds/YourFeed`.
 * *Subscribe* - Name of the topic you want to receive data from in the form
   `YourUserName/feeds/YourFeed`.

On the Authentication tab:
 * *Client ID* - can be anything, but try to keep it unique among connectors.
 * *Auth* should be set to *Username+Password*:
   * *Name* - Your Adafruit account name (see the last article in the list above).
   * *Password/Key* - Your Adafruit Key, NOT your account password (see above).

After all of this is ready, you need to select this Handler as a *Group* on your
*Devices* or *Nodes* configuration page. And **don't forget** to set your *Application*
field to **backend** there.
