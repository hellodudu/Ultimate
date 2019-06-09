FROM alpine
RUN apk add tzdata

RUN mkdir /app
WORKDIR /app

RUN /bin/cp /usr/share/zoneinfo/Asia/Shanghai /etc/localtime && echo 'Asia/Shanghai' >/etc/timezone

ADD ultimate-service-invite /app/ultimate-service-invite

ENTRYPOINT [ "/app/ultimate-service-invite" ]
