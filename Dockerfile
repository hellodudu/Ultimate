FROM alpine
# FROM mysql
COPY main main
COPY config config
COPY sql sql
EXPOSE 7030
