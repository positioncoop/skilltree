FROM postgres:9.3
RUN pg_createcluster
RUN service postgresql start
RUN psql -c "CREATE DATABASE skilltree_devel"
RUN psql -c "CREATE USER skilltree_user WITH PASSWORD '111'"
RUN psql -c "GRANT ALL ON DATABASE skilltree_devel TO skilltree_user"
EXPOSE 5432
CMD ["postgres"]