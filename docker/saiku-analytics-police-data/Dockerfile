FROM ojbc/saiku

WORKDIR /tmp

COPY files/server-config.sh /tmp/
COPY files/mondrian-connections.json /tmp/
COPY files/PoliceDataMondrianSchema.xml /tmp/

RUN chmod a+x /tmp/server-config.sh
RUN /tmp/server-config.sh

RUN cd /opt/saiku-server/tomcat

CMD ["/opt/saiku-server/start-saiku.sh"]