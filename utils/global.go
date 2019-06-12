package global

import (
	"strconv"

	"github.com/hellodudu/Ultimate/res"
)

// redis
var RedisAddr string = "127.0.0.1:6379"
var RedisPwd string = ""
var RedisDB int = 0

// inimgr
var IniMgr = res.NewIniMgr()

func turnToInt(v string, e error) int {
	n, _ := strconv.Atoi(v)
	return n
}

// world config
var (
	WorldHeartBeatSec  = turnToInt(IniMgr.GetIniValue("config/ultimate.ini", "world config", "WorldHeartBeatSec"))
	WorldConTimeOutSec = turnToInt(IniMgr.GetIniValue("config/ultimate.ini", "world config", "WorldConTimeOutSec"))
	WorldConnectMax    = uint32(turnToInt(IniMgr.GetIniValue("config/ultimate.ini", "world config", "WorldConnectMax")))
)

// mysql
var (
	MysqlUser, _ = IniMgr.GetIniValue("config/ultimate.ini", "mysql", "user")
	MysqlPwd, _  = IniMgr.GetIniValue("config/ultimate.ini", "mysql", "pwd")
	MysqlAddr, _ = IniMgr.GetIniValue("config/ultimate.ini", "mysql", "addr")
	MysqlPort, _ = IniMgr.GetIniValue("config/ultimate.ini", "mysql", "port")
	MysqlDB, _   = IniMgr.GetIniValue("config/ultimate.ini", "mysql", "db")
)

// ultimate
var UltimateID = turnToInt(IniMgr.GetIniValue("config/ultimate.ini", "ultimate", "id"))
var Debugging = func() bool {
	d := turnToInt(IniMgr.GetIniValue("config/ultimate.ini", "ultimate", "debug"))
	if d == 1 {
		return true
	}
	return false
}()
