FROM ojbc/mysql

COPY files/server-config.sh /tmp
COPY files/ojbc_analytics_police_data.sql.gz /tmp
RUN chmod a+x /tmp/server-config.sh
RUN /tmp/server-config.sh
