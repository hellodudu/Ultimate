package config

import "github.com/gorilla/websocket"

var TcpListenAddr string = "192.168.2.124:7030"
var HttpListenAddr string = "127.0.0.1:8080"
var WorldHeartBeatSec uint32 = 20
var WorldConTimeOutSec uint32 = 60
var WorldConnectMax uint32 = 500

var MysqlDSN string = "root:hello1986@tcp(127.0.0.1:3306)/comt"

var RedisAddr string = "127.0.0.1:6379"
var RedisPwd string = ""
var RedisDB int = 0

var Upgrader = websocket.Upgrader{
	ReadBufferSize:  1024,
	WriteBufferSize: 1024,
}
