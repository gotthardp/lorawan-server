# docker build -t gotthardp/lorawan-server .
# docker run --restart unless-stopped --detach --name lorawan --hostname lorawan \
#   --volume /root/storage:/storage --publish 8081:8080/tcp --publish 1680:1680/udp gotthardp/lorawan-server
FROM tedsluis/raspbian.minimal.arm7
RUN apt-get update && apt-get install -y wget git npm make

RUN apt-get install -y libssl-dev
RUN apt-get install -y ncurses-dev
RUN  wget http://www.erlang.org/download/otp_src_20.0.tar.gz
RUN  tar -xzvf otp_src_20.0.tar.gz
RUN  cd otp_src_20.0/ && ./configure && make && make install

RUN rm otp_src_20.0.tar.gz
RUN rm -R otp_src_20.0/
RUN mkdir lorawan-server
RUN wget https://github.com/gotthardp/lorawan-server/releases/download/v0.4.12/lorawan-server-0.4.12.tar.gz
RUN mv lorawan-server-0.4.12.tar.gz lorawan-server/
RUN cd lorawan-server && tar -zxvf lorawan-server-0.4.12.tar.gz

# volume for the mnesia database and logs
RUN mkdir -p /storage
VOLUME /storage
# data from port_forwarders
EXPOSE 1680/udp
# admin interface
EXPOSE 8080/tcp
ENV LORAWAN_HOME=/storage
WORKDIR /lorawan-server
CMD bin/lorawan-server
