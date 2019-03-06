package global

import (
	"strconv"

	"github.com/gorilla/websocket"
	"github.com/hellodudu/Ultimate/res"
)

// redis
var RedisAddr string = "127.0.0.1:6379"
var RedisPwd string = ""
var RedisDB int = 0

// websocket
var Upgrader = websocket.Upgrader{
	ReadBufferSize:  1024,
	WriteBufferSize: 1024,
}

// inimgr
var IniMgr = res.NewIniMgr()

func turnToInt(v string, e error) int {
	n, _ := strconv.Atoi(v)
	return n
}

// world config
var (
	WorldHeartBeatSec  = turnToInt(IniMgr.GetIniValue("config/config.ini", "world config", "WorldHeartBeatSec"))
	WorldConTimeOutSec = turnToInt(IniMgr.GetIniValue("config/config.ini", "world config", "WorldConTimeOutSec"))
	WorldConnectMax    = uint32(turnToInt(IniMgr.GetIniValue("config/config.ini", "world config", "WorldConnectMax")))
)

// mysql
var (
	MysqlUser, _ = IniMgr.GetIniValue("config/config.ini", "mysql", "user")
	MysqlPwd, _  = IniMgr.GetIniValue("config/config.ini", "mysql", "pwd")
	MysqlAddr, _ = IniMgr.GetIniValue("config/config.ini", "mysql", "addr")
	MysqlPort, _ = IniMgr.GetIniValue("config/config.ini", "mysql", "port")
	MysqlDB, _   = IniMgr.GetIniValue("config/config.ini", "mysql", "db")
)

// ultimate
var UltimateID = turnToInt(IniMgr.GetIniValue("config/config.ini", "ultimate", "id"))
