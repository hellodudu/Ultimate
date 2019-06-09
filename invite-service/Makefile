
GOPATH:=$(shell go env GOPATH)


.PHONY: proto
proto:
	protoc -I=../proto --go_out=plugins=micro:../proto/ ../proto/invite/invite.proto

.PHONY: build
build: proto

	go build -o ultimate-service-invite main.go plugin.go

.PHONY: test
test:
	go test -v ./... -cover

.PHONY: docker
docker:
	docker build . -t ultimate-service-invite:latest
