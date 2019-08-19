module github.com/hellodudu/Ultimate/arena-service

go 1.12

require (
	code.cloudfoundry.org/bytefmt v0.0.0-20190710193110-1eb035ffe2b6 // indirect
	github.com/NYTimes/gziphandler v1.1.1 // indirect
	github.com/docker/spdystream v0.0.0-20181023171402-6480d4af844c // indirect
	github.com/elazarl/goproxy v0.0.0-20190711103511-473e67f1d7d2 // indirect
	github.com/emicklei/go-restful v2.9.6+incompatible // indirect
	github.com/evanphx/json-patch v4.5.0+incompatible // indirect
	github.com/go-openapi/spec v0.19.2 // indirect
	github.com/go-openapi/swag v0.19.4 // indirect
	github.com/go-sql-driver/mysql v1.4.1
	github.com/golang/protobuf v1.3.2
	github.com/googleapis/gax-go v2.0.4+incompatible // indirect
	github.com/gophercloud/gophercloud v0.2.0 // indirect
	github.com/gregjones/httpcache v0.0.0-20190611155906-901d90724c79 // indirect
	github.com/hellodudu/Ultimate v0.0.0-20190620075824-e0382f9185c2
	github.com/hellodudu/Ultimate/game-service v0.0.0-20190819101416-1c9ac340075e // indirect
	github.com/jinzhu/gorm v1.9.10
	github.com/mailru/easyjson v0.0.0-20190626092158-b2ccc519800e // indirect
	github.com/micro/examples v0.2.0 // indirect
	github.com/micro/go-log v0.1.0
	github.com/micro/go-micro v1.8.1
	github.com/micro/go-plugins v1.2.0
	github.com/micro/util v0.2.0 // indirect
	github.com/munnerz/goautoneg v0.0.0-20190414153302-2ae31c8b6b30 // indirect
	github.com/pierrec/lz4 v2.2.4+incompatible // indirect
	github.com/schollz/progressbar/v2 v2.13.2 // indirect
	gotest.tools v2.3.0+incompatible // indirect
	k8s.io/client-go v12.0.0+incompatible // indirect
	k8s.io/gengo v0.0.0-20190327210449-e17681d19d3a // indirect
	sigs.k8s.io/structured-merge-diff v0.0.0-20190711200306-eaa53bff5a75 // indirect
)

replace github.com/hellodudu/Ultimate => ../

replace github.com/testcontainers/testcontainer-go => github.com/testcontainers/testcontainers-go v0.0.4

replace github.com/nats-io/nats.go v1.8.2-0.20190607221125-9f4d16fe7c2d => github.com/nats-io/nats.go v1.8.1

replace github.com/hashicorp/consul => github.com/hashicorp/consul v1.5.1
