
# GOPATH:=$(shell go env GOPATH)
v ?= latest

.PHONY: build
build:
	env GOOS=linux GOARCH=amd64 go build -o ultimate-service
	docker-compose build

.PHONY: proto
proto:
	protoc -I=proto --go_out=plugins=grpc:proto proto/world_message.proto

.PHONY: test
test:
	go test -v ./... -cover

.PHONY: run
run:
	docker run -itd -v $(shell pwd)/config:/app/config/ -v $(shell pwd)/log:/app/log/ -p 7030:7030 -p 8088:8080 ultimate-service:$(v)

.PHONY: docker_push
docker_push:
	docker tag ultimate hellodudu86/ultimate:$(v)
	docker push hellodudu86/ultimate:$(v)

.PHONY: docker_rm
docker_rm:
	docker rmi hellodudu86/ultimate:$(v)
	docker rmi ultimate:$(v)

.PHONY: stop
stop:
	docker stop $(shell docker ps -q  --filter ancestor=ultimate)
