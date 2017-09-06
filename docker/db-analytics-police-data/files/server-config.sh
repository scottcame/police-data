#! /bin/bash

set -x

cd /tmp

/usr/bin/mysqld_safe --user=root &

until /usr/bin/mysqladmin -u root status > /dev/null 2>&1; do sleep 1; done

echo "CREATE USER 'analytics'@'%' IDENTIFIED BY ''" | mysql -u root
echo "GRANT ALL PRIVILEGES ON *.* TO 'analytics'@'%' WITH GRANT OPTION" | mysql -u root

gunzip ojbc_analytics_police_data.sql.gz

echo "create database ojbc_analytics_police_data" | mysql -u root
mysql -u root ojbc_analytics_police_data < /tmp/ojbc_analytics_police_data.sql
rm -f /tmp/ojbc_analytics_police_data.sql
