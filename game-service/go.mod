module github.com/hellodudu/Ultimate/game-service

go 1.12

require (
	github.com/go-redis/redis v6.15.2+incompatible
	github.com/go-sql-driver/mysql v1.4.1
	github.com/golang/protobuf v1.3.1
	github.com/hellodudu/Ultimate v0.0.0-20190620075824-e0382f9185c2
	github.com/jinzhu/gorm v1.9.8
	github.com/micro/go-micro v1.5.0
	github.com/micro/go-plugins v1.1.0
	github.com/nats-io/nats-server/v2 v2.0.0 // indirect
	github.com/sirupsen/logrus v1.4.2
)

replace github.com/hellodudu/Ultimate => ../

replace github.com/golang/lint => golang.org/x/lint v0.0.0-20190409202823-959b441ac422

replace github.com/testcontainers/testcontainer-go => github.com/testcontainers/testcontainers-go v0.0.4
