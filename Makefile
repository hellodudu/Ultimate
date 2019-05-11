
GOPATH:=$(shell go env GOPATH)
v ?= latest

.PHONY: build
build:
	env GOOS=linux GOARCH=amd64 go build main.go
	docker build -t ultimate .

.PHONY: proto
proto:
	protoc -I=proto --go_out=plugins=grpc:proto proto/world_message.proto

.PHONY: test
test:
	go test -v ./... -cover

.PHONY: run
run:
	docker run -it -v $(shell pwd)/config:/app/config/ -v $(shell pwd)/log:/app/log/ -p 7030:7030 -p 8088:8080 ultimate:$(v)

