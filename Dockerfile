FROM erlang:20-slim
MAINTAINER Petr Gotthard <petr.gotthard@centrum.cz>

RUN echo "deb http://deb.nodesource.com/node_6.x stretch main" > /etc/apt/sources.list && apt-get update && apt-get install -y wget git nodejs && rm -r /var/cache/
RUN git clone https://github.com/gotthardp/lorawan-server.git && cd lorawan-server && make release && make clean

# volume for the mnesia database and logs
RUN mkdir /storage
VOLUME /storage

# data from port_forwarders
EXPOSE 1680/udp
# admin interface
EXPOSE 8080/tcp

ENV LORAWAN_HOME=/storage
WORKDIR /lorawan-server/_build/default/rel/lorawan-server
CMD bin/lorawan-server
