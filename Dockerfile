FROM alpine:3.8
RUN apk add tzdata

RUN mkdir /app
WORKDIR /app

RUN /bin/cp /usr/share/zoneinfo/Asia/Shanghai /etc/localtime && echo 'Asia/Shanghai' >/etc/timezone

COPY ultimate-service /app/ultimate-service

ENTRYPOINT ["/app/ultimate-service"]
