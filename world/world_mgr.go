package world

import (
	"context"
	"errors"
	"fmt"
	"net"
	"sync"

	"github.com/golang/protobuf/proto"
	"github.com/hellodudu/Ultimate/global"
	"github.com/hellodudu/Ultimate/logger"
)

type WorldMgr struct {
	mapWorld      sync.Map
	mapConn       sync.Map
	mapRefWorldID sync.Map
	wg            sync.WaitGroup
	ctx           context.Context
	cancel        context.CancelFunc
	chTimeOutW    chan uint32
	chStop        chan struct{}
}

func NewWorldMgr() (*WorldMgr, error) {
	w := &WorldMgr{
		chTimeOutW: make(chan uint32, global.WorldConnectMax),
		chStop:     make(chan struct{}, 1),
	}

	w.ctx, w.cancel = context.WithCancel(context.Background())

	return w, nil
}

func (ws *WorldMgr) Stop() chan struct{} {
	ws.mapWorld.Range(func(_, v interface{}) bool {
		if world, ok := v.(*World); ok {
			world.Stop()
		}
		return true
	})

	ws.cancel()
	return ws.chStop
}

func (ws *WorldMgr) AddWorld(id uint32, name string, con net.Conn) (*World, error) {
	var invalidID int32 = -1
	if id == uint32(invalidID) {
		return nil, errors.New("add world id invalid!")
	}

	if _, ok := ws.mapWorld.Load(id); ok {
		ws.KickWorld(id)
	}

	if _, ok := ws.mapConn.Load(con); ok {
		ws.KickWorld(id)
	}

	var numConn uint32 = 0
	ws.mapConn.Range(func(_, _ interface{}) bool {
		numConn++
		return true
	})

	if numConn >= global.WorldConnectMax {
		return nil, errors.New("world connected num full!")
	}

	w := NewWorld(id, name, con, ws.chTimeOutW)
	ws.mapWorld.Store(w.Id, w)
	ws.mapConn.Store(w.Con, w)
	logger.Info(fmt.Sprintf("add world <id:%d, name:%s, con:%v> success!", w.Id, w.Name, w.Con))
	go w.Run()
	return w, nil
}

func (ws *WorldMgr) AddWorldRef(id uint32, ref []uint32) {
	for _, v := range ref {
		ws.mapRefWorldID.Store(v, id)
	}
}

func (ws *WorldMgr) getWorldRefID(id uint32) uint32 {
	if v, ok := ws.mapRefWorldID.Load(id); ok {
		return v.(uint32)
	}

	return 0
}

func (ws *WorldMgr) GetWorldByID(id uint32) *World {
	worldID := ws.getWorldRefID(id)
	v, ok := ws.mapWorld.Load(worldID)
	if !ok {
		return nil
	}

	world, ok := v.(*World)
	if !ok {
		return nil
	}

	return world
}

func (ws *WorldMgr) GetWorldByCon(con net.Conn) *World {
	v, ok := ws.mapConn.Load(con)
	if !ok {
		return nil
	}

	world, ok := v.(*World)
	if !ok {
		return nil
	}

	return world
}

func (ws *WorldMgr) DisconnectWorld(con net.Conn) {
	v, ok := ws.mapConn.Load(con)
	if !ok {
		return
	}

	world, ok := v.(*World)
	if !ok {
		return
	}

	logger.Warning(fmt.Sprintf("World<id:%d> disconnected!", world.Id))
	world.Stop()

	ws.mapWorld.Delete(world.Id)
	ws.mapConn.Delete(con)
}

func (ws *WorldMgr) KickWorld(id uint32) {
	v, ok := ws.mapWorld.Load(id)
	if !ok {
		return
	}

	world, ok := v.(*World)
	if !ok {
		return
	}

	if _, ok := ws.mapConn.Load(world.Con); !ok {
		return
	}

	logger.Warning(fmt.Sprintf("World<id:%d> was kicked by timeout reason!", world.Id))
	world.Stop()

	ws.mapConn.Delete(world.Con)
	ws.mapWorld.Delete(world.Id)
}

func (ws *WorldMgr) BroadCast(msg proto.Message) {
	ws.mapWorld.Range(func(_, v interface{}) bool {
		if world, ok := v.(*World); ok {
			world.SendProtoMessage(msg)
		}
		return true
	})
}

func (ws *WorldMgr) Run() {
	for {
		select {
		case <-ws.ctx.Done():
			logger.Print("world session context done!")
			ws.chStop <- struct{}{}
			return
		case wid := <-ws.chTimeOutW:
			ws.KickWorld(wid)
		}
	}
}
