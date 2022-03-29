# Server Administration

## Users

Define a list of user identities that can manage the server.

For each User you can set:
 - **Name** and **Password** used to log-in.
 - **Scopes** the user is authorized for. When empty, the *unlimited* scope is assumed.
 - **E-Mail** where the status alerts can be sent.
 - **Send Alerts** flag indicating whether this user actually wants to receive
   status alerts via e-mail.

The following scopes are defined:
 - *unlimited* enables both read and write access to everything
 - *web-admin* covers access to the administration web-pages (html, css and javascript)
 - *server:read* and *server:write* covers the entire user and server configuration
 - *network:read* and *network:write* covers the entire area, gateway and network configuration
 - *device:read* and *device:write* covers the entire group, profile, device and node configuration
 - *backend:read* and *backend:write* covers the entire handler and connector configuration
 - *gateway:link* covers ability to connect as a gateway and send/receive frames
 - *device:send* covers ability to initiate downlinks to a device

Internal applications may add their specific scopes.


## Servers

The list shows a single line describing the current server instance, or multiple
lines describing the [Cluster](Cluster.md) nodes. For each it shows:
 - **Name** of the server node, which is useful for [debugging](Development.md#debugging)
 - **Version** of the server
 - **Free Memory** and **Free Disk**
 - **Alerts** that may need your attention:
   * `system_memory_high_watermark` when more than 80% of available system memory
     is allocated
   * `process_memory_high_watermark` when any Erlang process has allocated more
     than 5% of total system memory
   * `disk_almost_full` if any disk partition uses more than 80% of the available
     space

The Status tab displays:
 - **Alerts**
 - **Performance** of the server in the number of requests and errors per minute
 - **Free Memory**
 - **Disks** of the server and their utilization


## Configuration

This is a server-server configuration. You can set the following parameters:
 - **Admin URL** with `http://` or `https://` and server IP:port. This shall be
   the base address, after which `/admin` (or whatever defined by
   `http_admin_path`) can be appended to access the web-admin, or `/api` can be
   appended to access the REST API.
 - **Items Per Page** defines maximal length of each web-admin list. Clear this
   value to enable infinite paging.
 - **Slack Token** that identifies the Slack group
 - **Send events to** allows you to specify a [Handler](Handlers.md) to receive
   all events, including global server and gateway events that can't be captured
   otherwise. *Warning!* This handler's attached connectors *must not* have device
   specific wildcards, like `{devaddr}`, in their **Publish Events** parameter.

In the E-Mail you can set:
 - **From** address, like `noreply@example.com`.
 - **SMTP Server** address
 - **User** and **Password** for authentication towards the SMTP Server


## Events

This list includes an overview of errors and warnings that have occurred during
the last 24 hours. Each specific error or warning is listed only once. Complete
listing of errors and warnings is in the server logs.

Each record contains
 - **Severity** of the issue: error, warning or info.
 - **First Occurred** timestamp
 - **Last Occurred** timestamp, which gets updated every time this event occurs.
 - **Count** of occurrences, which gets increased by 1.
 - **Entity** concerned by the event: server, gateway, device, node, connector.
 - **EId**, which is an identifier of the concerned entity.
 - **Text** and **Args** of the event.

Description of the various errors and warnings is provided in the
[Troubleshooting Guide](Troubleshooting.md).
