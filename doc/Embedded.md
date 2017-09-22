# Installation to Embedded Systems

In any case you will need a POSIX compliant operating system with "enough"
resources (CPU and memory).

## Raspberry Pi

When using Raspbian you can use the standard `.deb` package available for every
[release](https://github.com/gotthardp/lorawan-server/releases).


## Multitech mLinux

First, follow the
[guidelines](http://www.multitech.net/developer/software/mlinux/mlinux-building-images/building-a-custom-linux-image/)
to setup the mLinux build environment:
```bash
git clone git://git.multitech.net/mlinux mlinux-3.x
cd mlinux-3.x
git checkout 3.3.9
ROOT_PASSWORD="root" ./setup.sh
```

Then, download the required metalayers:
```bash
cd layers
git clone https://github.com/joaohf/meta-erlang.git
git clone https://github.com/gotthardp/meta-lorawan.git
```
add them to the `conf/bblayers.conf`:

```bash
BBLAYERS ?= " \
    ...
    ${TOPDIR}/layers/meta-erlang \
    ${TOPDIR}/layers/meta-lorawan \
    "
```

And build the image
```bash
source env-oe.sh
bitbake mlinux-lorawan-image
```

Finally, follow the
[guidelines](http://www.multitech.net/developer/software/mlinux/using-mlinux/flashing-mlinux-firmware-for-conduit/)
to flash the mLinux Firmware:
```bash
mkdir /var/volatile/flash-upgrade
cp <uImage path here> /var/volatile/flash-upgrade/uImage.bin
cp <rootfs path here> /var/volatile/flash-upgrade/rootfs.jffs2
touch /var/volatile/do_flash_upgrade
reboot
```

Make sure your LoRa card was correctly detected by
```bash
mts-io-sysfs show lora/hw-version
```

Set `ENABLED="yes"` in `/etc/default/lorawan-server`.

Edit server configuration in `/usr/lib/lorawan-server/releases/<version>/sys.config`
and set `{disksup_posix_only, true}` since mLinux uses stripped-down Unix tools.


## OpenWRT

See the [MatchX blog](https://matchx.io/community/box/5-lorawan-server-running-on-the-box).
