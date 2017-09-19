DESCRIPTION = "Compact server for private LoRa networks"
HOMEPAGE = "https://gotthardp.github.io/lorawan-server/"
SECTION = "console/utils"
# https://github.com/joaohf/meta-erlang
DEPENDS = "erlang"

LICENSE = "MIT"
LIC_FILES_CHKSUM = "file://${COMMON_LICENSE_DIR}/MIT;md5=0835ade698e0bcf8506ecda2f7b4f302"

SRC_URI = "https://github.com/gotthardp/lorawan-server/archive/v${PV}.tar.gz"
SRC_URI[md5sum] = "4a131ae96e5554bcd76025d86e10e5ad"
SRC_URI[sha256sum] = "6bcf5141db8603758618048cc68979f9a2d44ed9fb923ce5450b390838a7dc7f"

S = "${WORKDIR}/${PN}-${PV}"

RDEPENDS_${PN} += "erlang erlang-compiler erlang-syntax-tools erlang-crypto erlang-inets \
    erlang-asn1 erlang-public-key erlang-ssl erlang-mnesia erlang-os-mon"

inherit useradd

USERADD_PACKAGES = "${PN}"
USERADD_PARAM_${PN} = "--home-dir /var/lib/lorawan-server --create-home lorawan"

do_compile() {
    oe_runmake release
}

do_install() {
    mkdir -p ${D}${libdir}
    cp -r ${S}/_build/default/rel/lorawan-server ${D}${libdir}
}
