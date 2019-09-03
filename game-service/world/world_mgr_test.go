package world

import (
	"io"
	"os"
	"testing"

	datastore "github.com/hellodudu/Ultimate/game-service/db"
	pbWorld "github.com/hellodudu/Ultimate/proto/world"
	"github.com/hellodudu/Ultimate/utils/global"
	logger "github.com/sirupsen/logrus"
)

type TestCon struct {
	Out io.Writer
}

func (c *TestCon) Close() {

}

func (c *TestCon) Write(b []byte) (n int, err error) {
	return c.Out.Write(b)
}

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

func TestNewWorldMgr(t *testing.T) {

	ds, err := datastore.NewDatastore()
	if err != nil {
		t.Error("NewDatastore error:", err)
	}

	wm, err := NewWorldMgr(ds)
	if err != nil {
		t.Error("NewWorldMgr error:", err)
	}

	// new world
	c1 := &TestCon{Out: os.Stdout}
	world1, err := wm.AddWorld(1, "world1", c1)
	if err != nil {
		t.Error("AddWorld error:", err)
	}

	c2 := &TestCon{Out: os.Stdout}
	if _, err := wm.AddWorld(2, "world2", c2); err != nil {
		t.Error("AddWorld error:", err)
	}

	// broadcast
	msg := &pbWorld.MWU_HeartBeat{}
	wm.BroadCast(msg)

	// add world ref
	wm.AddWorldRef(1, []uint32{11, 12, 13})
	wm.AddWorldRef(2, []uint32{21, 22, 23})

	if w := wm.GetWorldByID(11); w == nil {
		t.Error("GetWorldByID error")
	}

	if w := wm.GetWorldByID(23); w == nil {
		t.Error("GetWorldByID error")
	}

	// kick world
	wm.KickWorld(2)
	if w := wm.GetWorldByID(22); w != nil {
		t.Error("KickWorld error")
	}

	// disconnect world
	wm.DisconnectWorld(world1.GetCon())
	if w := wm.GetWorldByCon(c1); w != nil {
		t.Error("DisconnectWorld error")
	}
}
