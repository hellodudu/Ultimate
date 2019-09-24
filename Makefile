
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
	v=${v} docker-compose up -d

.PHONY: docker_push
docker_push:
	docker tag ultimate hellodudu86/ultimate:$(v)
	docker push hellodudu86/ultimate:$(v)

.PHONY: clean
clean:
	docker rm -f $(shell docker ps -a -q)

.PHONY: stop
stop:
	docker-compose down
