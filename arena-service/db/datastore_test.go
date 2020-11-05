package datastore

import (
	"testing"

	"github.com/hellodudu/Ultimate/iface"
	"github.com/hellodudu/Ultimate/utils/global"
)

var ds iface.IDatastore

func init() {
	global.Debugging = false
	global.MysqlUser = "root"
	global.MysqlPwd = ""
	global.MysqlAddr = "127.0.0.1"
	global.MysqlPort = "3306"
	global.MysqlDB = "db_ultimate"
	global.UltimateID = 110
}

func TestNewDatastore(t *testing.T) {

	var err error
	ds, err = NewDatastore()
	if err != nil {
		t.Error("NewDatastore error:", err)
	}

}

func TestTableGlobal(t *testing.T) {
	tbl := ds.TableGlobal()
	if tbl == nil {
		t.Error("Init table global error")
	}

	if tbl.Id != 110 {
		t.Error("Init table global ultimate_id error")
	}
}
