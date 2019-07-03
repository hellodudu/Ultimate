package server

import (
	"testing"

	"github.com/hellodudu/Ultimate/logger"
	"github.com/hellodudu/Ultimate/utils/global"
)

func init() {
	global.Debugging = false
	global.MysqlUser = "root"
	global.MysqlPwd = ""
	global.MysqlAddr = "127.0.0.1"
	global.MysqlPort = "3306"
	global.MysqlDB = "db_ultimate"
	global.UltimateID = 110
	global.WorldConnectMax = 500
	global.WorldHeartBeatSec = 10
	global.WorldConTimeOutSec = 10
	logger.Init(global.Debugging, false, "game_datastore_test")
}

func TestUltimate(t *testing.T) {
	umt, err := NewUltimate()
	if err != nil {
		t.Error("NewUltimate error:", err)
	}
}
