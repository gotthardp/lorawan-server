# Server Administration

You can administrate and manage the server via a set of web-pages or via a REST API.
By default, the server listens on HTTP port 8080 and expects "admin" as both username and password.
You can access it via a web browser by entering the URL `http://server:8080`, where
`server` is the IP or the hostname of your server.

The port and default credentials (which are set when the server database is created)
can be changed in the [`sys.config`](../lorawan_server.config). The credentials can
be then altered via the admin interface.

The server administration can be operated in an isolated network, except (Google)
graphs displayed in the Web Admin. Without internet connectivity the graphs will
not be displayed, but other Web Admin functions and the REST API will work fine.


## Web Admin

The management web-pages are available under `/admin`. It is a wrapper around
the REST API described below.

The server Dashboard shows:
 * Rolling timeline displaying recent Frames and [Events](Events.md).
 * Server information and a list of [Gateways](Infrastructure.md),
   [Devices](Devices.md) and [Nodes](Nodes.md) that may need your attention.
   The lines are sorted by severity.
 * Seven most recent [Events](Events.md) and seven most recent frames received.

The following configuration pages are available:
 * *Users* contain a list of user identities that can manage the server. All
   have the same access rights.
 * [*Infrastructure*](Infrastructure.md) covers configuration of LoRa Gateways,
   Multicast Channels and the list of Ignored Nodes.
 * [*Devices*](Devices.md) contain a list of devices that are allowed to join
   using the over-the-air-activation (OTAA).
 * [*Nodes*](Nodes.md) contain a list of active network nodes, both
   activated by personalization (ABP) as well as those that joined as OTAA.
 * [*Backends*](Backends.md) at remote servers that shall receive the
   application data.

You (at least) have to:
 * Add LoRaWAN gateways you want to use to the *Gateways* list.
 * Configure each device you want to use:
   * To add a device activated by personalization (ABP), create a new *Nodes* list entry.
   * To add an OTAA device, create a new *Devices* list entry and start the device.
     The *Nodes* list will be updated automatically once the device joins the network.


## REST API

The following REST resources are made available:

  Resource                  | Methods          | Explanation
 ---------------------------|------------------| ------------------------------------------------
  /servers                  | GET              | Server status information
  /applications             | GET              | Supported LoRaWAN applications
  /users                    | GET, POST        | Users of the admin interface
  /users/*ABC*              | GET, PUT, DELETE | User *ABC*
  /gateways                 | GET, POST        | LoRaWAN gateways
  /gateways/*123*           | GET, PUT, DELETE | Gateway with MAC=*123*
  /multicast_channels       | GET, POST        | Class C multicast channels
  /multicast_channels/*123* | GET, PUT, DELETE | Multicast channel with DevAddr=*123*
  /devices                  | GET, POST        | Devices registered for over-the-air activation (OTAA)
  /devices/*123*            | GET, PUT, DELETE | Device with DevEUI=*123*
  /nodes                    | GET, POST        | Active network nodes, both ABP and activated OTAA
  /nodes/*123*              | GET, PUT, DELETE | Active network node with DevAddr=*123*
  /ignored_nodes            | GET, POST        | Nodes ignored by the server
  /ignored_nodes/*123*      | GET, PUT, DELETE | Ignored node with DevAddr=*123*
  /txframes                 | GET              | Frames scheduled for transmission
  /txframes/*123*           | GET, DELETE      | Frame with ID=*123*
  /rxframes                 | GET              | Recent received frames
  /handlers                 | GET              | Backend handlers
  /handlers/*ABC*           | GET, DELETE      | Backend handler for the Group *ABC*
  /connectors               | GET              | Backend connectors
  /connectors/*ABC*         | GET, DELETE      | Backend connector *ABC*
  /events                   | GET              | Recent errors and warnings
  /upload                   | PUT              | Uploads (certificate) files to the server

There is a 1:1 mapping between the REST API and the Web Admin. Parameters
that are in the Web Admin indicated as optional doesn't need to be provided in
the REST API either.

As a rule, POST should be used to create new entries and PUT should be used to
update existing entries.

For example:

Get a list of all users by:

```HTTP
GET /users HTTP/1.1
```

```HTTP
HTTP/1.1 200 OK
Content-Type: application/json

[{"name":"admin","pass":"admin"},{"name":"backup","pass":"backup"}]
```

Create or update a set of users by:

```HTTP
POST /users HTTP/1.1
Content-Type: application/json

[{"name":"admin","pass":"admin"},{"name":"backup","pass":"backup"}]
```

```HTTP
HTTP/1.1 204 No Content
```

Get one user by:

```HTTP
GET /users/backup HTTP/1.1
```

```HTTP
HTTP/1.1 200 OK
Content-Type: application/json

{"name":"backup","pass":"backup"}
```

Update one user by:

```HTTP
PUT /users/backup HTTP/1.1
Content-Type: application/json

{"name":"backup","pass":"backup"}
```

```HTTP
HTTP/1.1 204 No Content
```

Delete one user by:

```HTTP
DELETE /users/backup HTTP/1.1
```

```HTTP
HTTP/1.1 204 No Content
```

### Filtering

To list only some items the REST API accepts the `_filters` query parameter, which
shall contain URL encoded JSON. For instance:

http://server:8080/rxframes?_filters={"devaddr":"22222222"}

### Sorting
The REST API accepts `_sortField` and `_sortDir` query parameters to sort the list. The
`_sortDir` can be either `ASC` or `DESC`. For instance:

http://server:8080/rxframes?_sortField=datetime&_sortDir=ASC

### Pagination
The REST API accepts `_page` and `_perPage` query parameters to paginate lists,
for instance:

http://server:8080/rxframes?_page=2&_perPage=20

The server also inserts the HTTP header `X-Total-Count` indicating the total item count.


## Proxy Configuration

When the lorawan-server is installed behind a HTTP proxy you may need to adjust
the proxy configuration as follows.

### Apache HTTP

```ApacheConf
<VirtualHost *:80>
    ProxyPass        /ws  ws://localhost:8080/ws
    ProxyPass        /    http://localhost:8080/
    ProxyPassReverse /    http://localhost:8080/
    ProxyPreserveHost On
    ServerName lorawan.example.com
</VirtualHost>
```


## Backup and Restore

Use the `bin/dbexport` script to backup your database. This will create a `backup-xxx`
directory with several `*.json` files. Use the `bin/dbimport backup-xxx` script to
write these files back to the server database.

The database is stored in the `Mnesia.lorawan@localhost` directory. To upgrade
the database structure or recover from database errors you should do `dbexport`,
then shutdown the server, update the server binaries, delete the Mnesia directory,
start the server and do `dbimport`.
