# Server Administration

You can administrate and manage the server via a set of web-pages or via a REST API.
By default, the server listens on HTTP port 8080 and expects "admin" as both username and password.

The port and default credentials (which are set when the server database is created)
can be changed in the [`sys.config`](lorawan_server.config). The credentials can
be then altered via the admin interface.

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

You (at least) have to:
 * Add LoRaWAN gateways you want to use to the *Gateways* list.
 * Configure each device you want to use:
   * To add a personalized device, create a new *Links* list entry.
   * To add an OTAA device, create a new *Devices* list entry and start the device. The *Links*
     list will be updated automatically once the device joins the network.

### Users

List of user identities that can manage the server. All have the same access rights.

### Gateways

For each LoRaWAN gateway you can set:
 * *MAC* address of the gateway
 * *NetID* of the network
 * *Location* and *Altitude* of the gateway

![alt tag](https://raw.githubusercontent.com/gotthardp/lorawan-server/master/doc/images/admin-gateway.png)

### Devices

For each device, which may connect to your network, you can set:
 * *DevEUI* of the device
 * *Application* identifier corresponding to one of the [Handlers](Handlers.md) configured.
 * *AppID*, which is a string with application-specific configuration.
 * *AppEUI* and *AppKey*

Once the device joins the network, the *Link* field will contain a reference to the *Links* list.

Optionally, you can also define a set of [ADR](ADR.md) parameters. Once the device
joins the network, the server will attempt to configure the device accordingly.

![alt tag](https://raw.githubusercontent.com/gotthardp/lorawan-server/master/doc/images/admin-device.png)

### Links

For each device, which is connected (has a link) to the network, you can set:
 * *DevEUI* of the device
 * *Application* identifier corresponding to one of the [Handlers](Handlers.md) configured.
 * *AppID*, which is a string with application-specific configuration.
 * *NwkSKey* and *AppSKey*

Optionally, you can also set the [ADR](ADR.md) parameters. The server will attempt
to configure the device accordingly.

Below the configuration options you can monitor the performance of the device. You
can see the assumed [ADR](ADR.md) parameters and two graphs that display the last
50 received frames.

The *Downlinks* table lists frames created by the application, which are scheduled for
transmission. Class A devices listen for downlinks only for 2 seconds after an uplink
transmission, so it may take a while until all messages are transmitted.

![alt tag](https://raw.githubusercontent.com/gotthardp/lorawan-server/master/doc/images/admin-link-status.png)
