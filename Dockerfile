FROM erlang:21-alpine
MAINTAINER Petr Gotthard <petr.gotthard@centrum.cz>

RUN apk add --no-cache --virtual build-deps git make wget nodejs-npm && \
    git clone https://github.com/gotthardp/lorawan-server.git && \
    cd lorawan-server && \
    make release install && \
    cd .. && \
    rm -rf lorawan-server && \
    apk del build-deps

# volume for the mnesia database and logs
RUN mkdir /storage
VOLUME /storage

# data from port_forwarders
EXPOSE 1680/udp
# http admin interface
EXPOSE 8080/tcp
# https admin interface
EXPOSE 8443/tcp

ENV LORAWAN_HOME=/storage
WORKDIR /usr/lib/lorawan-server
CMD bin/lorawan-server
