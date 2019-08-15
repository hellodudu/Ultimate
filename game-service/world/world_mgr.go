package world

import (
	"context"
	"errors"
	"fmt"
	"sync"
	"time"

	"github.com/golang/protobuf/proto"
	"github.com/hellodudu/Ultimate/iface"
	"github.com/hellodudu/Ultimate/utils/global"
	logger "github.com/hellodudu/Ultimate/utils/log"
)

type WorldMgr struct {
	mapWorld      sync.Map
	mapConn       sync.Map
	mapRefWorldID sync.Map
	ds            iface.IDatastore
	wg            sync.WaitGroup
	ctx           context.Context
	cancel        context.CancelFunc
	chTimeOutW    chan uint32
	chStop        chan struct{}
}

func NewWorldMgr(datastore iface.IDatastore) (*WorldMgr, error) {
	wm := &WorldMgr{
		chTimeOutW: make(chan uint32, global.WorldConnectMax),
		chStop:     make(chan struct{}, 1),
		ds:         datastore,
	}

	wm.ctx, wm.cancel = context.WithCancel(context.Background())
	wm.ds.DB().Set("gorm:table_options", "ENGINE=InnoDB DEFAULT CHARSET=utf8mb4").AutoMigrate(world{})

	return wm, nil
}

func (wm *WorldMgr) Stop() {
	wm.mapWorld.Range(func(_, v interface{}) bool {
		v.(*world).Stop()
		return true
	})

	wm.cancel()
	<-wm.chStop
	close(wm.chStop)
	close(wm.chTimeOutW)
}

func (wm *WorldMgr) AddWorld(id uint32, name string, con iface.ITCPConn) (iface.IWorld, error) {
	var invalidID int32 = -1
	if id == uint32(invalidID) {
		return nil, errors.New("add world id invalid!")
	}

	if _, ok := wm.mapWorld.Load(id); ok {
		wm.KickWorld(id, "AddWorld")
	}

	var numConn uint32
	wm.mapConn.Range(func(_, _ interface{}) bool {
		numConn++
		return true
	})

	if numConn >= uint32(global.WorldConnectMax) {
		return nil, errors.New("world connected num full!")
	}

	// new world
	w := NewWorld(id, name, con, wm.chTimeOutW, wm.ds)
	wm.mapWorld.Store(w.GetID(), w)
	wm.mapConn.Store(w.GetCon(), w)
	logger.Info(fmt.Sprintf("add world <id:%d, name:%s, con:%v> success!", w.GetID(), w.GetName(), w.GetCon()))

	// world run
	go w.Run()

	w.SetLastConTime(int(time.Now().Unix()))
	wm.ds.DB().Save(w)

	return w, nil
}

func (wm *WorldMgr) AddWorldRef(id uint32, ref []uint32) {
	for _, v := range ref {
		wm.mapRefWorldID.Store(v, id)
	}
}

func (wm *WorldMgr) getWorldRefID(id uint32) uint32 {
	if v, ok := wm.mapRefWorldID.Load(id); ok {
		return v.(uint32)
	}

	return 0
}

func (wm *WorldMgr) GetWorldByID(id uint32) iface.IWorld {
	worldID := wm.getWorldRefID(id)
	v, ok := wm.mapWorld.Load(worldID)
	if !ok {
		return nil
	}

	return v.(*world)
}

func (wm *WorldMgr) GetWorldByCon(con iface.ITCPConn) iface.IWorld {
	v, ok := wm.mapConn.Load(con)
	if !ok {
		return nil
	}

	return v.(*world)
}

func (wm *WorldMgr) DisconnectWorld(con iface.ITCPConn) {
	v, ok := wm.mapConn.Load(con)
	if !ok {
		return
	}

	world, ok := v.(*world)
	if !ok {
		return
	}

	logger.WithFieldsWarn("World disconnected!", logger.Fields{
		"id": world.GetID(),
	})
	world.Stop()

	wm.mapWorld.Delete(world.GetID())
	wm.mapConn.Delete(con)
}

func (wm *WorldMgr) KickWorld(id uint32, reason string) {
	v, ok := wm.mapWorld.Load(id)
	if !ok {
		return
	}

	world, ok := v.(*world)
	if !ok {
		return
	}

	logger.WithFieldsWarn("World was kicked!", logger.Fields{
		"id":     world.GetID(),
		"reason": reason,
	})

	world.Stop()
	wm.mapConn.Delete(world.GetCon())
	wm.mapWorld.Delete(world.GetID())
}

func (wm *WorldMgr) BroadCast(msg proto.Message) {
	wm.mapWorld.Range(func(_, v interface{}) bool {
		if world, ok := v.(*world); ok {
			world.SendProtoMessage(msg)
		}
		return true
	})
}

func (wm *WorldMgr) Run() {
	for {
		select {
		case <-wm.ctx.Done():
			logger.Print("world session context done!")
			wm.chStop <- struct{}{}
			return
		case wid := <-wm.chTimeOutW:
			wm.KickWorld(wid, "time out")
		}
	}
}
