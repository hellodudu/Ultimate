package config

import "github.com/gorilla/websocket"

var TcpListenAddr string = "192.168.2.124:7030"
var HttpListenAddr string = "127.0.0.1:8080"
var WorldHeartBeatSec uint32 = 20
var WorldConTimeOutSec uint32 = 60

var Upgrader = websocket.Upgrader{
	ReadBufferSize:  1024,
	WriteBufferSize: 1024,
}
