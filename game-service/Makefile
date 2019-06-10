.PHONY: proto
proto:
	protoc -I=../proto --go_out=plugins=micro:../proto/ ../proto/game/game.proto

.PHONY: build
build: proto
	env GOOS=linux GOARCH=amd64 go build -o ultimate-service-game main.go plugin.go

.PHONY: test
test:
	go test -v ./... -cover

.PHONY: docker
docker:
	docker build . -t ultimate-service-game:latest

.PHONY: run
run:
	docker run -it -p 50052:50052 \
	 -e MICRO_SERVER_ADDRESS=:50052 \
	 -e MICRO_REGISTRY=mdns \
	 ultimate-service-game
