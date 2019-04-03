FROM postgres:11.2

RUN mkdir -p /tmp/psql_data/

COPY backup.sql /tmp/psql_data/
COPY init.sh /docker-entrypoint-initdb.d/

RUN pg_createcluster 11 main --start

RUN touch /var/log/test.log

CMD /etc/init.d/postgresql start && tail -F /var/log/test.log

# OMEFIREs-MacBook-Pro:database omefire$ docker run -d --rm --name pg -p 5432:5432 omefire/trello-reminders-db