# Cluster

** WARNING: This code is under development and is not sufficiently tested! **

## Mode of operation

This LoRaWAN server implements and supports a *Simple Cluster*. Meaning that you
can have a number of network servers sharing their databases sitting in a LAN
behind a health sensing load balancer that distributes messages, received from
your LoRa gateways, among them. Database sharing and synchronization is done
solely by the means of Mnesia, the Erlang standard database.

It is advisable, but not mandatory, that your load balancer implements the
*sticky* mode, where a particular gateway, being initially forwarded to one of
the servers, will be forwarded to it with all later frames unless the server
becomes unavailable or its load becomes unacceptable.

Monitoring servers by the load balancer depends on its abilities and can be
anything from simple ICMP ping test to the HTTP REST querying of the server state,
see the [Administration Guide](Administration.md) for the details.

[TBD: insert picture here].

## Configuration

Before you start:
 - Plan your cluster and deploy the load balancer to manage the frames forwarded
   from the gateways to the servers.
 - Make sure the `.erlang.cookie` is the same on all nodes of your planned cluster.
 - Make sure each node has a unique host hostname (see `hostname -s`).
 - Make sure the short hostnames of your nodes can be resolved on the other nodes
   (`ping yourhost`).

Create the first server of your cluster. You don't need to do any special
configuration here, the server should be started as if it is a single standard server.

To add new servers to the cluster you can use either the web-admin or the
db_master configuration option.

### Add new nodes using the web-admin

Start the new node(s), then log-in to the web-admin on the master node, navigate
to the Servers List and click Create. Enter the name of the new node and click
Submit. This will **delete the database on the new node** and add the node to
the cluster.

### Join using the db_master config option

For every new server **before you start it up** uncomment in the servers
`sys.config` file the option `db_master` and specify any already running server
node as its value. For example:

```
{db_master, lorawan@alreadyrunning},
```

After that, when this new server starts up, it will initially contact the specified
server node and pull all the required data from it and you may safely remove this
option from your config file or just leave it there - it will never be used again,
it is only needed for the first start.

Mnesia database automatically detects any new members of the cluster and
propagates that information to every other server in it.

### Remove node using the web-admin

Log-in to the web-admin on the master node, navigate to the Server List and open
the server node you want to remove. The click the Delete button and confirm the
warning. This will terminate the removed node.

If you have any problems, ideas or suggestions, be sure to contact
[LoRaWAN Server Users](mailto:lorawan-server@googlegroups.com) mailing list.
