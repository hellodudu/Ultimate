GOPATH:=$(shell go env GOPATH)
v ?= latest

.PHONY: build
build: 
	env GOOS=linux GOARCH=amd64 go build -o ultimate-service-arena main.go plugin.go

.PHONY: proto
proto:
	protoc -I=../proto --go_out=plugins=micro:../proto/ ../proto/arena/arena.proto

.PHONY: test
test:
	go test -v ./... -cover

.PHONY: docker
docker: build
	docker build . -t ultimate-service-arena:latest

.PHONY: push
push: build
	docker tag ultimate-service-arena hellodudu86/ultimate-service-arena:$(v)
	docker push hellodudu86/ultimate-service-arena:$(v)

.PHONY: run
run:
	docker run -it \
	 -p 50051 \
	 -p 3306 \
	 -e MICRO_BROKER=nsq \
	 -e MICRO_SERVER_ADDRESS=:50051 \
	 -e MICRO_REGISTRY=mdns \
	 -e MYSQL_ADDR=host.docker.internal \
	 -v $(shell pwd)/../config:/config/ \
	 -v $(shell pwd)/../log:/log/ \
	 ultimate-service-arena
