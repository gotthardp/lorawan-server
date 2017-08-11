DESCRIPTION = "Compact server for private LoRa networks"
HOMEPAGE = "https://gotthardp.github.io/lorawan-server/"
SECTION = "console/utils"
# https://github.com/joaohf/meta-erlang
DEPENDS = "erlang"

LICENSE = "MIT"
LIC_FILES_CHKSUM = "file://${COMMON_LICENSE_DIR}/MIT;md5=0835ade698e0bcf8506ecda2f7b4f302"

SRC_URI = "https://github.com/gotthardp/lorawan-server/archive/v${PV}.tar.gz"
SRC_URI[md5sum] = "b22f6e3510180ca960b6346c9483477d"
SRC_URI[sha256sum] = "fd46d092cc7fc9c98d615435e95e83757f1ce9f2b6277fb076fdac21902e06d8"

S = "${WORKDIR}/${PN}-${PV}"

RDEPENDS_${PN} += "erlang-compiler erlang-inets erlang-mnesia erlang-public-key erlang-runtime-tools erlang-ssl"

do_compile() {
    oe_runmake release
}

do_install() {
    mkdir -p ${D}${libdir}
    cp -r ${S}/_build/default/rel/lorawan-server ${D}${libdir}
}
