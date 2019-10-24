GOPATH:=$(shell go env GOPATH)

.PHONY: build
build: 
	env CGO_ENABLED=0 GOOS=linux GOARCH=amd64 go build -o rpc-client main.go plugin.go

.PHONY: docker
docker: build
	docker build . -t rpc-client:latest

.PHONY: run
run:
	go run main.go plugin.go
