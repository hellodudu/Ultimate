
# GOPATH:=$(shell go env GOPATH)
v ?= latest

.PHONY: build
build:
	make -C arena-service build
	make -C game-service build

.PHONY: proto
proto:
	make -C arena-service proto
	make -C game-service proto
	protoc -I=./proto --go_out=plugins=micro:${GOPATH}/src ./proto/pubsub/pubsub.proto

.PHONY: docker
docker:
	make -C arena-service docker
	make -C game-service docker

.PHONY: test
test:
	make -C arena-service test
	make -C game-service test
	go test -v ./... -cover

.PHONY: run
run:
	docker-compose up

.PHONY: push
push:
	make -C arena-service push
	make -C game-service push

.PHONY: docker_rm
docker_rm:
	docker rmi $(shell docker images -f "dangling=true" -q) --force

.PHONY: stop
stop:
	docker stop $(shell docker ps -q  --filter ancestor=ultimate)
