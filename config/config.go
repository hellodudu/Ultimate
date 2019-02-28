package config

import (
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

var im *IniMgr = nil

type IniMgr struct {
	mapIniFile map[string]*ini.File
}

// get ini file by path
func (im *IniMgr) getIniFile(name string) (*ini.File, error) {
	if f, ok := im.mapIniFile[name]; ok {
		return f, nil
	}

	f, err := ini.Load(name)
	if err == nil {
		im.mapIniFile[name] = f
		return f, nil
	}

	return nil, err
}

func GetIniMgr() *IniMgr {
	if im == nil {
		im = &IniMgr{
			mapIniFile: make(map[string]*ini.File, 256),
		}
	}

	return im
}

// get ini value by section and key
func (im *IniMgr) GetIniValue(name string, section string, key string) (string, error) {
	f, err := im.getIniFile(name)
	if err != nil {
		return "", err
	}

	s, err := f.GetSection(section)
	if err != nil {
		return "", err
	}

	k, err := s.GetKey(key)
	if err != nil {
		return "", err
	}

	return k.Value(), nil
}
