# Server Administration

You can administrate and manage the server via a set of web-pages or via a REST API.
By default, the server listens on HTTP port 8080 and expects "admin" as both username and password.

## REST API

The following REST resources are made available:

  Resource        | Methods          | Explanation
 -----------------|------------------| ------------------------------------------------
  /applications   | GET              | Supported LoRaWAN applications
  /users          | GET, POST        | Users of the admin interface
  /users/*ABC*    | GET, PUT, DELETE | User *ABC*
  /gateways       | GET, POST        | LoRaWAN gateways
  /gateways/*123* | GET, PUT, DELETE | Gateway with MAC=*123*
  /devices        | GET, POST        | Devices registered for over-the-air activation
  /devices/*123*  | GET, PUT, DELETE | Device with DevEUI=*123*
  /links          | GET, POST        | Activated devices
  /links/*123*    | GET, PUT, DELETE | Activated device with DevAddr=*123*
  /txframes       | GET              | Frames scheduled for transmission
  /txframes/*123* | GET, DELETE      | Frame with ID=*123*

## Web Admin

The management web-pages are available under `/admin`. It is just a wrapper around
the REST API.

To register a new gateway, create a new *Gateways* list entry.

To add a personalized device, create a new *Links* list entry.
To add an OTAA device, create a new *Devices* list entry and start the device. The *Links*
list will be updated automatically once the device joins the network.

![alt tag](https://raw.githubusercontent.com/gotthardp/lorawan-server/master/doc/admin.png)
