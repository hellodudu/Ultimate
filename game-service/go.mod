module github.com/hellodudu/Ultimate/game-service

go 1.12

require (
	github.com/go-redis/redis v6.15.2+incompatible
	github.com/go-sql-driver/mysql v1.4.1
	github.com/golang/protobuf v1.3.2
	github.com/hellodudu/Ultimate v0.0.0-20190620075824-e0382f9185c2
	github.com/jinzhu/gorm v1.9.8
	github.com/lib/pq v1.2.0 // indirect
	github.com/marten-seemann/qtls v0.3.1 // indirect
	github.com/micro/go-micro v1.7.1-0.20190711204633-5157241c88e0
	github.com/micro/go-plugins v1.1.2-0.20190710094942-bf407858372c
	github.com/nats-io/jwt v0.2.10 // indirect
	github.com/nats-io/nats-server/v2 v2.0.2 // indirect
	github.com/sirupsen/logrus v1.4.2
	github.com/smartystreets/goconvey v0.0.0-20190710185942-9d28bd7c0945 // indirect
	golang.org/x/sys v0.0.0-20190712062909-fae7ac547cb7 // indirect
	google.golang.org/genproto v0.0.0-20190716160619-c506a9f90610 // indirect
)

replace github.com/hellodudu/Ultimate => ../

replace github.com/golang/lint => golang.org/x/lint v0.0.0-20190409202823-959b441ac422

replace github.com/testcontainers/testcontainer-go => github.com/testcontainers/testcontainers-go v0.0.4
