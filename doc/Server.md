# Server Administration

## Users

Define a list of user identities that can manage the server.

For each User you can set:
 - **Name** and **Password** used to log-in.
 - **Roles**. For now only the `admin` *Role* is defined.
 - **E-Mail** where the status alerts can be sent.
 - **Send Alerts** flag indicating whether this user actually wants to receive
   status alerts via e-mail.


## Servers

The list shows a single line describing the current server instance:
 - **Name** of the cluster node, which is useful for [debugging](Development.md#debugging)
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
 - **Admin URL** included in E-Mail messages
 - **Items Per Page** defines maximal length of each web-admin list. Clear this
   value to enable infinite paging.
 - **Google API Key**, which has Google Maps enabled. You can
   [get your API Key](https://developers.google.com/maps/documentation/javascript/get-api-key) from Google.
 - **Slack Token** that identifies the Slack group

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
 - **Last Occurred** timestamp, which gets updated everytime this event occurs.
 - **Count** of occurrences, which gets increased by 1.
 - **Entity** concerned by the event: server, gateway, device, node, connector.
 - **EId**, which is an identifier of the concerned entity.
 - **Text** and **Args** of the event.

Description of the various errors and warnings is provided in the
[Troubleshooting Guide](Troubleshooting.md).
