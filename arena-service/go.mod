module github.com/hellodudu/Ultimate/arena-service

go 1.12

require (
	code.cloudfoundry.org/bytefmt v0.0.0-20190710193110-1eb035ffe2b6 // indirect
	contrib.go.opencensus.io/exporter/stackdriver v0.12.2 // indirect
	github.com/Azure/azure-pipeline-go v0.2.2 // indirect
	github.com/Azure/azure-sdk-for-go v31.1.0+incompatible // indirect
	github.com/Azure/azure-service-bus-go v0.8.0 // indirect
	github.com/Azure/azure-storage-blob-go v0.7.0 // indirect
	github.com/Azure/go-autorest v12.3.0+incompatible // indirect
	github.com/GoogleCloudPlatform/cloudsql-proxy v0.0.0-20190711182328-f0b00cc64402 // indirect
	github.com/NYTimes/gziphandler v1.1.1 // indirect
	github.com/RoaringBitmap/roaring v0.4.18 // indirect
	github.com/Shopify/sarama v1.23.0 // indirect
	github.com/anacrolix/tagflag v1.0.0 // indirect
	github.com/aws/aws-sdk-go v1.20.20 // indirect
	github.com/cenkalti/backoff v2.2.0+incompatible // indirect
	github.com/docker/spdystream v0.0.0-20181023171402-6480d4af844c // indirect
	github.com/elazarl/goproxy v0.0.0-20190711103511-473e67f1d7d2 // indirect
	github.com/emicklei/go-restful v2.9.6+incompatible // indirect
	github.com/evanphx/json-patch v4.5.0+incompatible // indirect
	github.com/glycerine/goconvey v0.0.0-20190410193231-58a59202ab31 // indirect
	github.com/go-openapi/spec v0.19.2 // indirect
	github.com/go-openapi/swag v0.19.4 // indirect
	github.com/go-sql-driver/mysql v1.4.1
	github.com/golang/groupcache v0.0.0-20190702054246-869f871628b6 // indirect
	github.com/golang/protobuf v1.3.2
	github.com/googleapis/gax-go v2.0.4+incompatible // indirect
	github.com/gophercloud/gophercloud v0.2.0 // indirect
	github.com/gregjones/httpcache v0.0.0-20190611155906-901d90724c79 // indirect
	github.com/grpc-ecosystem/grpc-gateway v1.9.4 // indirect
	github.com/hellodudu/Ultimate v0.0.0-20190620075824-e0382f9185c2
	github.com/hellodudu/Ultimate/game-service v0.0.0-20190730133608-509682c071eb // indirect
	github.com/jcmturner/gofork v1.0.0 // indirect
	github.com/jinzhu/gorm v1.9.10
	github.com/mailru/easyjson v0.0.0-20190626092158-b2ccc519800e // indirect
	github.com/micro/go-micro v1.8.1
	github.com/micro/go-plugins v1.1.2-0.20190710094942-bf407858372c
	github.com/munnerz/goautoneg v0.0.0-20190414153302-2ae31c8b6b30 // indirect
	github.com/nats-io/jwt v0.2.10 // indirect
	github.com/nats-io/nats-server/v2 v2.0.2 // indirect
	github.com/pierrec/lz4 v2.2.4+incompatible // indirect
	github.com/rcrowley/go-metrics v0.0.0-20190706150252-9beb055b7962 // indirect
	github.com/rogpeppe/fastuuid v1.2.0 // indirect
	github.com/schollz/progressbar/v2 v2.13.2 // indirect
	github.com/tidwall/pretty v1.0.0 // indirect
	go.mongodb.org/mongo-driver v1.0.4 // indirect
	gopkg.in/jcmturner/gokrb5.v7 v7.3.0 // indirect
	k8s.io/api v0.0.0-20190717022910-653c86b0609b // indirect
	k8s.io/client-go v12.0.0+incompatible // indirect
	k8s.io/gengo v0.0.0-20190327210449-e17681d19d3a // indirect
	k8s.io/utils v0.0.0-20190712204705-3dccf664f023 // indirect
	pack.ag/amqp v0.12.0 // indirect
	sigs.k8s.io/structured-merge-diff v0.0.0-20190711200306-eaa53bff5a75 // indirect
)

replace github.com/hellodudu/Ultimate => ../

replace github.com/testcontainers/testcontainer-go => github.com/testcontainers/testcontainers-go v0.0.4

replace github.com/nats-io/nats.go v1.8.2-0.20190607221125-9f4d16fe7c2d => github.com/nats-io/nats.go v1.8.1

replace github.com/hashicorp/consul => github.com/hashicorp/consul v1.5.1
