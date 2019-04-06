
### To build and run the postgres container
- docker build -t o .
- docker run --rm -dit -p 5432:5432 --name tdb o
- docker exec -it tdb bash

### To build and run the pgAdmin4 container
- docker pull dpage/pgadmin4
- docker run --link tdb -p 80:80 \
        -e "PGADMIN_DEFAULT_EMAIL=user@domain.com" \
        -e "PGADMIN_DEFAULT_PASSWORD=SuperSecret" \
        -d dpage/pgadmin4
- Go to http://localhost:80 and connect to the postgres server by entering 'tdb' as the server name and 'postgres' as username       
