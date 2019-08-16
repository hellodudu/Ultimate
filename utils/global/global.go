package global

import (
	"strconv"

	"github.com/hellodudu/Ultimate/utils"
)

// redis
var (
	RedisAddr = "127.0.0.1:6379"
	RedisPwd  string
	RedisDB   int
)

// inimgr
var (
	iniMgr    *utils.IniMgr
	Debugging bool
)

// world config
var (
	WorldHeartBeatSec  int
	WorldConTimeOutSec int
	WorldConnectMax    int
)

// mysql
var (
	MysqlUser string
	MysqlPwd  string
	MysqlAddr string
	MysqlPort string
	MysqlDB   string
)

// ultimate
var (
	UltimateID int
)

func init() {
	iniMgr = utils.NewIniMgr()
	Debugging = func() bool {
		d := turnToInt(iniMgr.GetIniValue("../config/ultimate.ini", "ultimate", "debug"))
		if d == 1 {
			return true
		}
		return false
	}()

	// world config
	WorldHeartBeatSec = turnToInt(iniMgr.GetIniValue("../config/ultimate.ini", "world config", "WorldHeartBeatSec"))
	WorldConTimeOutSec = turnToInt(iniMgr.GetIniValue("../config/ultimate.ini", "world config", "WorldConTimeOutSec"))
	WorldConnectMax = int(turnToInt(iniMgr.GetIniValue("../config/ultimate.ini", "world config", "WorldConnectMax")))

	// mysql
	MysqlUser, _ = iniMgr.GetIniValue("../config/ultimate.ini", "mysql", "user")
	MysqlPwd, _ = iniMgr.GetIniValue("../config/ultimate.ini", "mysql", "pwd")
	MysqlAddr, _ = iniMgr.GetIniValue("../config/ultimate.ini", "mysql", "addr")
	MysqlPort, _ = iniMgr.GetIniValue("../config/ultimate.ini", "mysql", "port")
	MysqlDB, _ = iniMgr.GetIniValue("../config/ultimate.ini", "mysql", "db")

	// ultimate
	UltimateID = turnToInt(iniMgr.GetIniValue("../config/ultimate.ini", "ultimate", "id"))
}

func turnToInt(v string, e error) int {
	n, _ := strconv.Atoi(v)
	return n
}

func GetIniMgr() *utils.IniMgr {
	return iniMgr
}
