package config

import (
	"log"

	"github.com/fatih/color"
	"github.com/go-ini/ini"
	"github.com/gorilla/websocket"
)

var HttpListenAddr string = ":8080"

// world config
var WorldHeartBeatSec uint32 = 20
var WorldConTimeOutSec uint32 = 60
var WorldConnectMax uint32 = 500

// mysql
var MysqlDSN string = "root:123456@tcp(127.0.0.1:3306)/db_ultimate"

// redis
var RedisAddr string = "127.0.0.1:6379"
var RedisPwd string = ""
var RedisDB int = 0

// websocket
var Upgrader = websocket.Upgrader{
	ReadBufferSize:  1024,
	WriteBufferSize: 1024,
}

// load ini file by path
func LoadIniFile(name string) (*ini.File, error) {
	return ini.Load(name)
}

// get ini value by section and key
func GetIniValue(f *ini.File, section string, key string) (string, error) {
	s, err := f.GetSection(section)
	if err != nil {
		log.Println(color.RedString("has no section named %s!", section))
		return "", err
	}

	k, err := s.GetKey(key)
	if err != nil {
		log.Println(color.RedString("has no key named %s!", key))
		return "", err
	}

	return k.Value(), nil
}
