package config

import "github.com/gorilla/websocket"

var TcpListenAddr string = "127.0.0.1:7030"
var HttpListenAddr string = "127.0.0.1:8080"

var Upgrader = websocket.Upgrader{
	ReadBufferSize:  1024,
	WriteBufferSize: 1024,
}
