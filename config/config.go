package config

import "github.com/gorilla/websocket"

// listen
var TcpListenAddr string = ":7030"
var HttpListenAddr string = ":8080"

// world config
var WorldHeartBeatSec uint32 = 20
var WorldConTimeOutSec uint32 = 60
var WorldConnectMax uint32 = 500

// mysql
var MysqlDSN string = "root:hello1986@tcp(127.0.0.1:3306)/db_ultimate"

// redis
var RedisAddr string = "127.0.0.1:6379"
var RedisPwd string = ""
var RedisDB int = 0

// websocket
var Upgrader = websocket.Upgrader{
	ReadBufferSize:  1024,
	WriteBufferSize: 1024,
}
