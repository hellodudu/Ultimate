FROM alpine
RUN apk add tzdata

RUN mkdir /app
WORKDIR /app

RUN /bin/cp /usr/share/zoneinfo/Asia/Shanghai /etc/localtime && echo 'Asia/Shanghai' >/etc/timezone

ADD ultimate-service-arena /app/ultimate-service-arena

ENTRYPOINT [ "/app/ultimate-service-arena" ]
