module github.com/hellodudu/Ultimate

go 1.12

require (
	github.com/BurntSushi/toml v0.3.1
	github.com/denisenkom/go-mssqldb v0.0.0-20190715232110-2b613d287457 // indirect
	github.com/gammazero/workerpool v0.0.0-20190608213748-0ed5e40ec55e
	github.com/go-ini/ini v1.44.0
	github.com/golang/protobuf v1.4.0
	github.com/jinzhu/gorm v1.9.10
	github.com/judwhite/go-svc v1.1.2
	github.com/mattn/go-sqlite3 v1.11.0 // indirect
	github.com/micro/cli v0.2.0
	github.com/micro/go-micro v1.8.1
	github.com/micro/go-plugins v1.2.0
	github.com/mreiferson/go-options v0.0.0-20190302064952-20ba7d382d05
	github.com/rs/zerolog v1.20.0
	google.golang.org/appengine v1.6.2 // indirect
	gopkg.in/ini.v1 v1.45.0 // indirect
	gopkg.in/natefinch/lumberjack.v2 v2.0.0

)

replace github.com/nats-io/nats.go v1.8.2-0.20190607221125-9f4d16fe7c2d => github.com/nats-io/nats.go v1.8.1
