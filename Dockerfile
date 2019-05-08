FROM alpine:3.8
RUN apk add tzdata

RUN mkdir /app
WORKDIR /app

COPY main /app/main

RUN /bin/cp /usr/share/zoneinfo/Asia/Shanghai /etc/localtime \
    && echo 'Asia/Shanghai' >/etc/timezone 

ENTRYPOINT ["/app/main"]
