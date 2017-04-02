# Integration Guide

## Generic MQTT Server

You can integrate with generic MQTT server, e.g. the
[RabbitMQ](https://www.rabbitmq.com/mqtt.html).

Open the lorawan-server web-administration and create an Application Connector:
 * *URI* defines the target host either as `mqtt://host:port` or `mqtts://host:port`
 * *Auth* shall be set to *Username+Password*, even when the *Name* and
   *Password/Key* are empty.

Then create a new Handler:
 * *Connector ID* shall point to the *Application Connector* you just created.
 * *Outbound* is a publication topic.
 * *Inbound* is a subscription topic.

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

Then, open the lorawan-server web-administration and create an Application Connector:
 * *URI* is the AWS *Endpoint* with the `mqtts://` prefix
 * *Client ID* is the AWS *Account Id*
 * *Auth* shall be set to *Username+Password*
 * *User Certificate* is the `xxx-certificate.pem.crt` file you downloaded
 * *Private Key* is the `xxx-private.pem.key` file

Finally, create a new Handler:
 * *Connector ID* shall point to the *Application Connector* you just created.
 * *Outbound* is a publication topic.
 * *Inbound* is a subscription topic.

## Azure IoT Hub

Microsoft Azure IoT Hub can be integrated via MQTT. A separate connection is
needed for each Node.

First, follow the Azure guidelines to configure your IoT device:
 * Create a new Device, use *Symmetric Key* authentication and let the system to
   *Auto Generate Keys*.
 * Optionally you may also define a *Shared access policy*.

Then, open the lorawan-server web-administration and create an Application Connector:
 * *URI* is the IoT Hub *Hostname* with the `mqtts://` prefix
 * *Client ID* is the *Device ID*
 * *Auth* shall be set to *Shared Access Signature*
 * When authenticating using the device key:
   * *Name* shall be empty
   * *Password/Key* is the device *Primary key* (encoded using Base64)
 * When authenticating using a *Shared access policy*:
   * *Name* is the *Access policy name*
   * *Password/Key* is the access policy *Primary key* (encoded using Base64)

Finally, create a new Handler:
 * *Connector ID* shall point to the *Application Connector* you just created.
 * *Outbound* shall be "devices/*Device ID*/messages/events/".
   The trailing slash is mandatory.
 * *Inbound* shall be "devices/*Device ID*/messages/devicebound/#".

## Adafruit IO

AdafruitIO supports MQTT and MQTT/SSL. Before doing the integration, make sure
you consult the following Adafruit articles:

* [Adafruit IO](https://learn.adafruit.com/adafruit-io)
* [Adafruit IO Basics: Feeds](https://learn.adafruit.com/adafruit-io-basics-feeds)
* [Adafruit IO Basics: Dashboards](https://learn.adafruit.com/adafruit-io-basics-dashboards)
* [MQTT, AdafruitIO & You!](https://learn.adafruit.com/mqtt-adafruit-io-and-you)

Once your Adafruit account, dashboards and feeds are set up, go to the
lorawan-server web-administration and create an Applications->Connector:
* *Connector ID* - Your name for this connector, should be unique.
* *URI* - `mqtt://io.adafruit.com` or `mqtts://io.adafruit.com`.
* *Client ID* - can be anything, but try to keep it unique among connectors.
* *Auth* should be set to *Username+Password*:
   * *Name* - Your Adafruit account name (see the last article in the list above).
   * *Password/Key* - Your Adafruit Key, NOT your account password (see above).

Finally, create a new Applications->Handler:
* *AppID* - Name to identify your Handler in the *Devices* configuration.
* *Connector ID* - Should point to the Connector you just created.
* *Outbound* - Name of the topic you will be publishing to in the form
`YourUserName/feeds/YourFeed`.
* *Inbound* - Name of the topic you want to receive data from in the form
`YourUserName/feeds/YourFeed`.

After all of this is ready, you may select this Handler as an *AppID* in your
*Devices* or *Nodes* configuration pages. And **don't forget** to set your *Application*
field to **connector** there.
