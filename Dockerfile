FROM alpine:3.8
FROM mysql:8.0
COPY main main
# COPY config config
# COPY sql sql
ENTRYPOINT ["./main"]
