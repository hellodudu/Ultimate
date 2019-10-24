GOPATH:=$(shell go env GOPATH)
v ?= latest

.PHONY: build
build: 
	env CGO_ENABLED=0 GOOS=linux GOARCH=amd64 go build -o ultimate-service-game main.go plugin.go

.PHONY: proto
proto:
	protoc -I=../proto --go_out=:${GOPATH}/src --micro_out=:${GOPATH}/src ../proto/game/game.proto
	protoc -I=../proto --go_out=:${GOPATH}/src --micro_out=:${GOPATH}/src ../proto/world/world.proto

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
	go run main.go plugin.go
	
