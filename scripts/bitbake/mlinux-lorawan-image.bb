require recipes-core/images/mlinux-base-image.bb

IMAGE_INSTALL += "\
    mc \
    lora-packet-forwarder \
    lorawan-server \
"
