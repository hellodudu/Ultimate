module github.com/hellodudu/Ultimate/game-service

go 1.12

require (
	github.com/go-redis/redis v6.15.2+incompatible
	github.com/go-sql-driver/mysql v1.4.1
	github.com/golang/protobuf v1.3.2
	github.com/google/btree v1.0.0 // indirect
	github.com/hellodudu/Ultimate v0.0.0-20190620075824-e0382f9185c2
	github.com/jinzhu/gorm v1.9.10
	github.com/micro/go-micro v1.8.1
	github.com/micro/go-plugins v1.2.0
	github.com/prometheus/client_golang v1.1.0
	github.com/sirupsen/logrus v1.4.2
)

replace github.com/hellodudu/Ultimate => ../

replace github.com/testcontainers/testcontainer-go => github.com/testcontainers/testcontainers-go v0.0.4

replace github.com/nats-io/nats.go v1.8.2-0.20190607221125-9f4d16fe7c2d => github.com/nats-io/nats.go v1.8.1

replace github.com/hashicorp/consul => github.com/hashicorp/consul v1.5.1
