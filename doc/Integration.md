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
