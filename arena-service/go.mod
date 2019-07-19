module github.com/hellodudu/Ultimate/arena-service

go 1.12

require (
	github.com/go-sql-driver/mysql v1.4.1
	github.com/golang/protobuf v1.3.2
	github.com/hellodudu/Ultimate v0.0.0-20190620075824-e0382f9185c2
	github.com/jinzhu/gorm v1.9.8
	github.com/micro/go-micro v1.7.1-0.20190627135301-d8e998ad85fe
	github.com/micro/go-plugins v1.1.1
)

replace github.com/hellodudu/Ultimate => ../

replace github.com/golang/lint => golang.org/x/lint v0.0.0-20190409202823-959b441ac422

replace github.com/testcontainers/testcontainer-go => github.com/testcontainers/testcontainers-go v0.0.4

replace github.com/hashicorp/consul => github.com/hashicorp/consul v1.5.1
