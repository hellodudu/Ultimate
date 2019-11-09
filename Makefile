
v ?= latest

.PHONY: build
build:
	make -C arena-service build
	make -C game-service build

.PHONY: proto
proto:
	make -C arena-service proto
	make -C game-service proto
	protoc -I=./proto --go_out=:${GOPATH}/src --micro_out=:${GOPATH}/src ./proto/pubsub/pubsub.proto

.PHONY: docker
docker:
	make -C arena-service docker
	make -C game-service docker
	make -C apps/rpc_presure docker

.PHONY: test
test:
	make -C arena-service test
	make -C game-service test

.PHONY: run
run:
	make -C arena-service run
	make -C game-service run
	make -C apps/rpc_presure run

.PHONY: docker_run
docker_run:
	v=${v} docker-compose up -d

.PHONY: push
push:
	make -C arena-service push
	make -C game-service push
	make -C apps/rpc_presure push

.PHONY: clean
clean:
	docker rm -f $(shell docker ps -a -q)


.PHONY: stop
stop:
	docker-compose down
