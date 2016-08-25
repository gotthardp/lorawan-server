FROM solarisbank/erlang:19.0
MAINTAINER Petr Gotthard <petr.gotthard@centrum.cz>

RUN apk --no-cache add nodejs curl git
RUN curl -o /usr/bin/rebar3 "https://s3.amazonaws.com/rebar3/rebar3" && chmod +x /usr/bin/rebar3
RUN git clone https://github.com/gotthardp/lorawan-server.git && cd lorawan-server && rebar3 release

# data from port_forwarders
EXPOSE 1680/udp
# admin interface
EXPOSE 8080/tcp

WORKDIR /lorawan-server/_build/default/rel/lorawan-server
CMD bin/lorawan-server foreground
