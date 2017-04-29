# Development Guidelines

## Software Architecture

The following figure shows a process hierarchy of the lorawan-server:

![alt tag](https://raw.githubusercontent.com/gotthardp/lorawan-server/master/doc/images/software-architecture.png)


## Debugging

### Logging

Using server logs is the most common technique to debug server functions.
Please review the [lager framework](https://github.com/basho/lager) documentation
for a comprehensive description.

For example, you can generate log messages by doing the following:
```erlang
lager:warning("unexpected value: ~w", [Value])
```

### Connections to the lorawan node

The lorawan-server is started in a cluster node `lorawan@<hostname>`. For example,
if your `hostname` is `debian.home`, the server runs on `lorawan@debian`.

You can call functions on another node by `rpc:call(Node, Module, Function, Args)`.

You can setup a connection to another node by `net_adm:ping(Node)` and then
interact with its processes.

Or you can start the [Observer](http://erlang.org/doc/apps/observer/observer_ug.html)
and just trace the lorawan-server processes and activities.

```bash
erl -smp -sname observer -hidden -setcookie MyCookie -run observer
```

Please note that nodes allowed to communicate with each other use the same
**magic cookie**. Make sure that `$HOME/.erlang.cookie` is the same, or
enter the cookie explicitly by the `-setcookie MyCookie` parameter.
