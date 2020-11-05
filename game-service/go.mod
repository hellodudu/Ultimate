module github.com/hellodudu/Ultimate/game-service

go 1.12

require (
	github.com/Azure/go-autorest/autorest v0.9.2 // indirect
	github.com/Azure/go-autorest/autorest/adal v0.8.0 // indirect
	github.com/Azure/go-autorest/autorest/to v0.3.0 // indirect
	github.com/Azure/go-autorest/autorest/validation v0.2.0 // indirect
	github.com/go-redis/redis v6.15.2+incompatible
	github.com/go-sql-driver/mysql v1.4.1
	github.com/golang/protobuf v1.4.0
	github.com/hashicorp/consul v0.0.0-00010101000000-000000000000 // indirect
	github.com/hashicorp/go-checkpoint v0.5.0 // indirect
	github.com/hellodudu/Ultimate v0.0.0-20190620075824-e0382f9185c2
	github.com/jinzhu/gorm v1.9.10
	github.com/micro/go-micro v1.18.0
	github.com/micro/go-micro/v2 v2.9.1
	github.com/micro/go-plugins v1.2.0
	github.com/micro/go-plugins/broker/nsq/v2 v2.9.1 // indirect
	github.com/micro/go-plugins/registry/consul v0.0.0-20200119172437-4fe21aa238fd
	github.com/micro/go-plugins/registry/consul/v2 v2.9.1 // indirect
	github.com/micro/go-plugins/transport/tcp/v2 v2.9.1 // indirect
	github.com/micro/go-plugins/wrapper/monitoring/prometheus/v2 v2.9.1
	github.com/prometheus/client_golang v1.5.1
	github.com/rs/zerolog v1.20.0
	github.com/sirupsen/logrus v1.4.2
)

replace github.com/hellodudu/Ultimate => ../

replace github.com/testcontainers/testcontainer-go => github.com/testcontainers/testcontainers-go v0.0.4

replace github.com/nats-io/nats.go v1.8.2-0.20190607221125-9f4d16fe7c2d => github.com/nats-io/nats.go v1.8.1

replace github.com/hashicorp/consul => github.com/hashicorp/consul v1.5.1
