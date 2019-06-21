module github.com/hellodudu/Ultimate/game-service

go 1.12

require (
	github.com/go-ini/ini v1.42.0 // indirect
	github.com/go-redis/redis v6.15.2+incompatible
	github.com/golang/protobuf v1.3.1
	github.com/hellodudu/Ultimate v0.0.0-20190620075824-e0382f9185c2
	github.com/jinzhu/gorm v1.9.8
	github.com/micro/go-micro v1.5.0
	github.com/sirupsen/logrus v1.4.2
)

replace github.com/hellodudu/Ultimate => ../
