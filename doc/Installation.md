# Server Installation

This document describes how to build, install and upgrade the lorawan-server.
After installation, please follow the [Configuration Instructions](Configuration.md)
to correctly setup your server.

## Installation

### Using the Debian package

On the Debian Linux and its clones like Raspbian you can use the .deb package.

Unless you have Debian 10 (Buster) you have to install the Erlang/OTP 21.0 or later from
[Erlang Solutions](https://www.erlang-solutions.com/resources/download.html) first:
```bash
wget https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb
sudo dpkg -i erlang-solutions_1.0_all.deb
```

```bash
sudo apt-get update
sudo apt-get install erlang-base erlang-crypto erlang-syntax-tools erlang-inets \
    erlang-mnesia erlang-runtime-tools erlang-ssl erlang-public-key erlang-asn1 \
    erlang-os-mon erlang-snmp erlang-xmerl
```

Download the Debian package
[lorawan-server-*.deb](https://github.com/gotthardp/lorawan-server/releases)
and install it by:
```bash
dpkg -i lorawan-server-<VERSION>.deb
```

If you want the server to start automatically after system reboot, run
`systemctl enable lorawan-server`.

Then start the server by `systemctl start lorawan-server`.

### Using the Binary Release on Linux

You will need the Erlang/OTP 21.0 or higher. Try typing `yum install erlang` or
`apt-get install erlang`.

Check your Erlang/OTP version by typing `erl`. If your Linux distribution
includes some older version of Erlang/OTP, install an update from
[Erlang Solutions](https://www.erlang-solutions.com/resources/download.html).

Then download the latest binary release
[lorawan-server-*.tar.gz](https://github.com/gotthardp/lorawan-server/releases)
and (on Linux) unpack it by:
```bash
mkdir lorawan-server
mv lorawan-server-<VERSION>.tar.gz lorawan-server/
cd lorawan-server
tar -zxvf lorawan-server-<VERSION>.tar.gz
```

You can run the server by:
```bash
bin/lorawan-server
```

By default the database directory `Mnesia.lorawan@*` is stored in
the `lorawan-server` directory you created. If you want to store the run-time
information to another directory, set the `LORAWAN_HOME` environment variable.

The `log` files are by default stored in the same directory. If you want to
store the log files elsewhere, set the `LORAWAN_LOG_ROOT` variable.

The lorawan-server can be started in background as a daemon.
On Linux systems with systemd you should:
 * Unpack the binary release to `/usr/lib/lorawan-server`
 * Copy `bin/lorawan-server.service` to `/lib/systemd/system`
 * Create a dedicated user by `useradd --home-dir /var/lib/lorawan-server --create-home lorawan`
 * Create the `/var/log/lorawan-server` directory and make sure it is owned by the user `lorawan`
 * Start the server by `systemctl start lorawan-server`

This will put the database into `/var/lib/lorawan-server` and server logs into
`/var/log/lorawan-server`.

### Using the Binary Release on Windows

Install the [32-bit or 64-bit Binary File](http://www.erlang.org/downloads) of
Erlang/OTP 21.0 or later.

Unpack the binary release
[lorawan-server-*.tar.gz](https://github.com/gotthardp/lorawan-server/releases)
using the [7-Zip](http://www.7-zip.org) to a new folder
(e.g. `lorawan-server`) and then run the server by double-clicking `lorawan-server.bat`
in the `lorawan-server/bin` folder.

You can also run the lorawan-server as a Windows service.
The service can be managed from a Command Prompt (`cmd`) using
`lorawan-service.bat <command>`, where the `<command>` could be:
 - **add** to add the service. Once added you can use the standard Windows control
   panel administrative tools to start/stop or enable/disable the service.
 - **remove** to remove the previously added service.
 - **list** to display parameters of a previously added service.

### Using the Binary Release on Mac OS

Install Erlang by `brew install erlang`.

Unpack and run the binary release
```bash
mkdir lorawan-server
mv lorawan-server-<VERSION>.tar.gz lorawan-server/
cd lorawan-server
tar -zxvf lorawan-server-<VERSION>.tar.gz
bin/lorawan-server
```


## Upgrade

The server binaries are stored in three subdirectories: `bin`, `lib` and `releases`.
These files are either:
 * In `/usr/lib/lorawan-server` when using the official releases
 * In `lorawan-server/_build/default/rel/lorawan-server` when you build the server
   from sources
 * Or wherever you extracted them

The server run-time files are automatically created during the first run. It
includes the database in `Mnesia.lorawan@*` and `log` files. These files are either:
 * In `/var/lib/lorawan-server` when using the official releases
 * In the directory specified by the `LORAWAN_HOME` environment variable
 * Otherwise it is in the same directory as the server binaries

To upgrade your server binaries:
 * Stop the lorawan-server
 * Backup or make sure you don't delete the `Mnesia.lorawan@*` sub-directory.
 * Delete the existing `bin`, `lib` and `releases` sub-directories and replace
   them by `bin`, `lib` and `releases` from the new binary release. You can simply
   unpack the content of the binary release e.g. in `/usr/lib/lorawan-server`.
 * Copy `bin/lorawan-server.service` to `/lib/systemd/system`
 * Reload the services by `sudo systemctl daemon-reload`
 * Start the lorawan-server


## Build Instructions

### Manual Installation

You will need the following prerequisites:
 * Rebar3, the Erlang build tool.
   * On Linux it will download automatically.
   * On Windows follow the [installation instructions](https://www.rebar3.org/docs/getting-started).
   * On Mac OS, run `brew install rebar`.
 * npm, the JavaScript package manager.
   * On Linux follow the instructions bellow.
   * On Windows, install the [Node.js](https://nodejs.org/en/).
   * On Mac OS, run `brew install node`.

Make sure you have the run-time prerequisites:

```bash
sudo apt-get update
sudo apt-get install policykit-1 make wget curl
sudo apt-get install erlang-base erlang-crypto erlang-syntax-tools erlang-inets \
    erlang-mnesia erlang-runtime-tools erlang-ssl erlang-public-key erlang-asn1 \
    erlang-os-mon erlang-snmp erlang-xmerl
```

Required `npm` can be installed from [here](https://github.com/nodesource/distributions).
On Debian, add the following to your `/etc/apt/sources.list` and run `apt-get update`
before installation:
```
deb http://deb.nodesource.com/node_6.x stretch main
deb-src http://deb.nodesource.com/node_6.x stretch main
```

Then, obtain the build prerequisites:
```bash
sudo apt-get install git erlang-dev erlang-parsetools erlang-src erlang-eunit \
     nodejs rsync
```

Get the latest lorawan-server sources by:
```bash
git clone https://github.com/gotthardp/lorawan-server.git
cd lorawan-server
```

If you already obtained the sources you can upgrade to the latest version by:
```bash
cd lorawan-server
git pull
make upgrade
```

Then build and release the lorawan-server by:
```bash
make release
```

The release will be created in `lorawan-server/_build/default/rel/lorawan-server`.

If you encounter issues with npm, please try to:
 * Update your npm with `sudo npm install -g npm`
 * Make sure github.com is listed in the list of known hosts by running `ssh github.com`,
   which will fail but will also add github.com to your `.ssh/known_hosts`:
   ```
   The authenticity of host 'github.com (192.30.253.113)' can't be established.
   RSA key fingerprint is SHA256:xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.
   Are you sure you want to continue connecting (yes/no)? yes
   Warning: Permanently added 'github.com,192.30.253.113' (RSA) to the list of known hosts.
   Permission denied (publickey).
   ```

According to the above installation instructions the server binaries are under
`/usr/lib/lorawan-server`. To upgrade your installation you shall **replace** the
content of the `bin`, `lib` and `releases` sub-directories with the newly created
content.

### Creating the Debian package

On the Debian Linux and its clones like Raspbian you can use the .deb package.

Build the Debian package bu running `make release dpkg`. This will create
`lorawan-server/_build/default/rel/lorawan-server/lorawan-server_<VERSION>.deb`.

You can then install the package by:
```bash
dpkg -i lorawan-server_*.deb
```

You can start the server by `systemctl start lorawan-server`.
