package world

import (
	"context"
	"errors"
	"fmt"
	"net"
	"sync"
	"time"

	"github.com/golang/protobuf/proto"
	"github.com/hellodudu/Ultimate/global"
	"github.com/hellodudu/Ultimate/iface"
	"github.com/hellodudu/Ultimate/logger"
)

type WorldMgr struct {
	mapWorld      sync.Map
	mapConn       sync.Map
	mapRefWorldID sync.Map
	datastore     iface.IDatastore
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
		datastore:  datastore,
	}

	wm.ctx, wm.cancel = context.WithCancel(context.Background())

	return wm, nil
}

func (wm *WorldMgr) Stop() chan struct{} {
	wm.mapWorld.Range(func(_, v interface{}) bool {
		if world, ok := v.(*World); ok {
			world.Stop()
		}
		return true
	})

	wm.cancel()
	return wm.chStop
}

func (wm *WorldMgr) AddWorld(id uint32, name string, con net.Conn) (*World, error) {
	var invalidID int32 = -1
	if id == uint32(invalidID) {
		return nil, errors.New("add world id invalid!")
	}

	if _, ok := wm.mapWorld.Load(id); ok {
		wm.KickWorld(id)
	}

	if _, ok := wm.mapConn.Load(con); ok {
		wm.KickWorld(id)
	}

	var numConn uint32 = 0
	wm.mapConn.Range(func(_, _ interface{}) bool {
		numConn++
		return true
	})

	if numConn >= global.WorldConnectMax {
		return nil, errors.New("world connected num full!")
	}

	// new world
	w := NewWorld(id, name, con, wm.chTimeOutW, wm.datastore)
	wm.mapWorld.Store(w.Id, w)
	wm.mapConn.Store(w.Con, w)
	logger.Info(fmt.Sprintf("add world <id:%d, name:%s, con:%v> success!", w.Id, w.Name, w.Con))

	// world run
	go w.Run()

	// save to db
	query := fmt.Sprintf("replace into world(id, name, last_connect_time) values(%d, \"%s\", %d)", world.ID(), world.Name(), int32(time.Now().Unix()))
	wm.datastore.Exec(query)

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

func (wm *WorldMgr) GetWorldByID(id uint32) *World {
	worldID := wm.getWorldRefID(id)
	v, ok := wm.mapWorld.Load(worldID)
	if !ok {
		return nil
	}

	world, ok := v.(*World)
	if !ok {
		return nil
	}

	return world
}

func (wm *WorldMgr) GetWorldByCon(con net.Conn) *World {
	v, ok := wm.mapConn.Load(con)
	if !ok {
		return nil
	}

	world, ok := v.(*World)
	if !ok {
		return nil
	}

	return world
}

func (wm *WorldMgr) DisconnectWorld(con net.Conn) {
	v, ok := wm.mapConn.Load(con)
	if !ok {
		return
	}

	world, ok := v.(*World)
	if !ok {
		return
	}

	logger.Warning(fmt.Sprintf("World<id:%d> disconnected!", world.Id))
	world.Stop()

	wm.mapWorld.Delete(world.Id)
	wm.mapConn.Delete(con)
}

func (wm *WorldMgr) KickWorld(id uint32) {
	v, ok := wm.mapWorld.Load(id)
	if !ok {
		return
	}

	world, ok := v.(*World)
	if !ok {
		return
	}

	if _, ok := wm.mapConn.Load(world.Con); !ok {
		return
	}

	logger.Warning(fmt.Sprintf("World<id:%d> was kicked by timeout reason!", world.Id))
	world.Stop()

	wm.mapConn.Delete(world.Con)
	wm.mapWorld.Delete(world.Id)
}

func (wm *WorldMgr) BroadCast(msg proto.Message) {
	wm.mapWorld.Range(func(_, v interface{}) bool {
		if world, ok := v.(*World); ok {
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
			wm.KickWorld(wid)
		}
	}
}
