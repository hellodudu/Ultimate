package datastore

import (
	"testing"

	"github.com/hellodudu/Ultimate/utils/global"
	logger "github.com/hellodudu/Ultimate/utils/log"
)

func init() {
	global.Debugging = false
	global.MysqlUser = "root"
	global.MysqlPwd = ""
	global.MysqlAddr = "127.0.0.1"
	global.MysqlPort = "3306"
	global.MysqlDB = "db_ultimate"
	global.UltimateID = 110

	logger.Init(global.Debugging, false, "game_datastore_test")
}

func TestNewDatastore(t *testing.T) {

	_, err := NewDatastore()
	if err != nil {
		t.Error("NewDatastore error:", err)
	}

}
