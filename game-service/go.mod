module github.com/hellodudu/Ultimate/game-service

go 1.12

require (
	github.com/Jeffail/gabs v1.1.1 // indirect
	github.com/SAP/go-hdb v0.13.2 // indirect
	github.com/SermoDigital/jose v0.9.2-0.20161205224733-f6df55f235c2 // indirect
	github.com/asaskevich/govalidator v0.0.0-20180720115003-f9ffefc3facf // indirect
	github.com/docker/docker v0.7.3-0.20190506211059-b20a14b54661 // indirect
	github.com/elazarl/go-bindata-assetfs v1.0.0 // indirect
	github.com/fatih/structs v1.1.0 // indirect
	github.com/go-redis/redis v6.15.2+incompatible
	github.com/go-sql-driver/mysql v1.4.1
	github.com/gocql/gocql v0.0.0-20190122205811-30de9a1866a8 // indirect
	github.com/golang/protobuf v1.3.2
	github.com/google/go-querystring v1.0.0 // indirect
	github.com/hashicorp/go-memdb v0.0.0-20181108192425-032f93b25bec // indirect
	github.com/hashicorp/go-plugin v0.0.0-20181212150838-f444068e8f5a // indirect
	github.com/hellodudu/Ultimate v0.0.0-20190620075824-e0382f9185c2
	github.com/jinzhu/gorm v1.9.10
	github.com/keybase/go-crypto v0.0.0-20181127160227-255a5089e85a // indirect
	github.com/micro/go-micro v1.7.1-0.20190711204633-5157241c88e0
	github.com/micro/go-plugins v1.1.2-0.20190710094942-bf407858372c
	github.com/mitchellh/copystructure v1.0.0 // indirect
	github.com/openzipkin/zipkin-go v0.1.6 // indirect
	github.com/patrickmn/go-cache v2.1.0+incompatible // indirect
	gopkg.in/mgo.v2 v2.0.0-20180705113604-9856a29383ce // indirect
)

replace github.com/hellodudu/Ultimate => ../

replace github.com/testcontainers/testcontainer-go => github.com/testcontainers/testcontainers-go v0.0.4

// replace github.com/nats-io/nats.go v1.8.2-0.20190607221125-9f4d16fe7c2d => github.com/nats-io/nats.go v1.8.1

replace github.com/hashicorp/consul => github.com/hashicorp/consul v1.5.1
