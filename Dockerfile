FROM solarisbank/erlang:19.0

RUN apk --no-cache add nodejs curl git
RUN curl -o /usr/bin/rebar3 "https://s3.amazonaws.com/rebar3/rebar3" && chmod +x /usr/bin/rebar3
RUN git clone https://github.com/gotthardp/lorawan-server.git && cd lorawan-server && rebar3 release

WORKDIR /lorawan-server/_build/default/rel/lorawan-server

CMD bin/lorawan-server foreground
