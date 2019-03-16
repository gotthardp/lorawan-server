################################################################################
#
# lorawan-server
#
################################################################################

LORAWAN_SERVER_VERSION = v0.6.7
LORAWAN_SERVER_SITE = $(call github,gotthardp,lorawan-server,$(LORAWAN_SERVER_VERSION))
LORAWAN_SERVER_LICENSE = MIT
LORAWAN_SERVER_LICENSE_FILES = LICENSE
LORAWAN_SERVER_INSTALL_STAGING = YES

define LORAWAN_SERVER_BUILD_CMDS
	$(MAKE) -C $(@D) $(HOST_CONFIGURE_OPTS) release
endef

define LORAWAN_SERVER_INSTALL_TARGET_CMDS
	$(MAKE) -C $(@D) DESTDIR=$(TARGET_DIR) install
endef

$(eval $(generic-package))
