module github.com/hellodudu/Ultimate/arena-service

go 1.12

require (
	github.com/go-sql-driver/mysql v1.4.1
	github.com/golang/protobuf v1.3.2
	github.com/hellodudu/Ultimate v0.0.0-20190620075824-e0382f9185c2
	github.com/hellodudu/shippy v0.0.0-20190519024118-629041b14978 // indirect
	github.com/jinzhu/gorm v1.9.10
	github.com/micro/go-micro v1.18.0
	github.com/micro/go-plugins v1.2.0
	github.com/micro/go-plugins/registry/consul v0.0.0-20200119172437-4fe21aa238fd // indirect
	github.com/micro/micro v1.16.0 // indirect
	github.com/sirupsen/logrus v1.4.2
)

replace github.com/hellodudu/Ultimate => ../

replace github.com/testcontainers/testcontainer-go => github.com/testcontainers/testcontainers-go v0.0.4

replace github.com/nats-io/nats.go v1.8.2-0.20190607221125-9f4d16fe7c2d => github.com/nats-io/nats.go v1.8.1

replace github.com/hashicorp/consul => github.com/hashicorp/consul v1.5.1
