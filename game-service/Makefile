GOPATH:=$(shell go env GOPATH)
v ?= latest

.PHONY: build
build: 
	env GOOS=linux GOARCH=amd64 go build -o ultimate-service-game main.go plugin.go

.PHONY: proto
proto:
	protoc -I=../proto --go_out=plugins=micro:${GOPATH}/src ../proto/game/game.proto
	protoc -I=../proto --go_out=plugins=micro:${GOPATH}/src ../proto/world/world.proto

.PHONY: test
test:
	go test -v ./... -cover

.PHONY: docker
docker: build
	docker build . -t ultimate-service-game:latest

.PHONY: push
push: build
	docker tag ultimate-service-game hellodudu86/ultimate-service-game:$(v)
	docker push hellodudu86/ultimate-service-game:$(v)

.PHONY: run
run:
	docker run -it \
	 -p 50052 \
	 -p 3306 \
	 -p 7030:7030 \
	 -e MICRO_SERVER_ADDRESS=:50052 \
	 -e MICRO_REGISTRY=mdns \
	 -e MYSQL_ADDR=host.docker.internal \
	 -v $(shell pwd)/../config:/config/ \
	 -v $(shell pwd)/../log:/log/ \
	 ultimate-service-game
