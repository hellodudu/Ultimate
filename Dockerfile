FROM alpine:3.8
FROM mysql:8.0
COPY main main
# COPY config config
# COPY sql sql
RUN /bin/cp /usr/share/zoneinfo/Asia/Shanghai /etc/localtime \
    && echo 'Asia/Shanghai' >/etc/timezone 
ENTRYPOINT ["./main"]
