GOPATH:=$(shell go env GOPATH)
v ?= latest

.PHONY: build
build: 
	env CGO_ENABLED=0 GOOS=linux GOARCH=amd64 go build -o rpc-client main.go plugin.go

.PHONY: docker
docker: build
	docker build . -t rpc-client:latest

.PHONY: run
run:
	go run main.go plugin.go

.PHONY: push
push: build
	docker tag rpc-client hellodudu86/rpc-client:$(v)
	docker push hellodudu86/rpc-client:$(v)


