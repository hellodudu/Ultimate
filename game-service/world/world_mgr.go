package world

import (
	"context"
	"errors"
	"fmt"
	"sync"
	"time"

	"github.com/golang/protobuf/proto"
	"github.com/hellodudu/Ultimate/global"
	"github.com/hellodudu/Ultimate/iface"
	log "github.com/sirupsen/logrus"
)

type WorldMgr struct {
	mapWorld      map[uint32]iface.IWorld
	mapConn       map[iface.ITCPConn]iface.IWorld
	mapRefWorldID map[uint32]uint32
	mu            sync.Mutex

	ds         iface.IDatastore
	wg         sync.WaitGroup
	ctx        context.Context
	cancel     context.CancelFunc
	chTimeOutW chan uint32
	chStop     chan struct{}
}

func NewWorldMgr(datastore iface.IDatastore) (*WorldMgr, error) {
	wm := &WorldMgr{
		mapWorld:      make(map[uint32]iface.IWorld),
		mapConn:       make(map[iface.ITCPConn]iface.IWorld),
		mapRefWorldID: make(map[uint32]uint32),
		chTimeOutW:    make(chan uint32, global.WorldConnectMax),
		chStop:        make(chan struct{}, 1),
		ds:            datastore,
	}

	wm.ctx, wm.cancel = context.WithCancel(context.Background())
	wm.ds.DB().Set("gorm:table_options", "ENGINE=InnoDB DEFAULT CHARSET=utf8mb4").AutoMigrate(world{})

	return wm, nil
}

func (wm *WorldMgr) Stop() chan struct{} {
	wm.mu.Lock()
	defer wm.mu.Unlock()
	for _, v := range wm.mapWorld {
		v.Stop()
	}

	wm.cancel()
	return wm.chStop
}

func (wm *WorldMgr) AddWorld(id uint32, name string, con iface.ITCPConn) (iface.IWorld, error) {
	var invalidID int32 = -1
	if id == uint32(invalidID) {
		return nil, errors.New("add world id invalid!")
	}

	if _, ok := wm.mapWorld[id]; ok {
		wm.KickWorld(id)
	}

	if _, ok := wm.mapConn[con]; ok {
		wm.KickWorld(id)
	}

	if len(wm.mapConn) >= global.WorldConnectMax {
		return nil, errors.New("world connected num full!")
	}

	// new world
	w := NewWorld(id, name, con, wm.chTimeOutW, wm.ds)

	wm.mu.Lock()
	wm.mapWorld[w.GetID()] = w
	wm.mapConn[w.GetCon()] = w
	wm.mu.Unlock()

	log.Info(fmt.Sprintf("add world <id:%d, name:%s, con:%v> success!", w.GetID(), w.GetName(), w.GetCon()))

	// world run
	go w.Run()

	w.SetLastConTime(int(time.Now().Unix()))
	wm.ds.DB().Save(w)

	return w, nil
}

func (wm *WorldMgr) AddWorldRef(id uint32, ref []uint32) {
	wm.mu.Lock()
	defer wm.mu.Unlock()

	for _, v := range ref {
		wm.mapRefWorldID[v] = id
	}
}

func (wm *WorldMgr) GetWorldByID(id uint32) iface.IWorld {
	worldID, ok := wm.mapRefWorldID[id]
	if !ok {
		return nil
	}

	w, ok := wm.mapWorld[worldID]
	if !ok {
		return nil
	}

	return w
}

func (wm *WorldMgr) GetWorldByCon(con iface.ITCPConn) iface.IWorld {
	w, ok := wm.mapConn[con]
	if !ok {
		return nil
	}

	return w
}

func (wm *WorldMgr) DisconnectWorld(con iface.ITCPConn) {
	w, ok := wm.mapConn[con]
	if !ok {
		return
	}

	log.Warn(fmt.Sprintf("World<id:%d> disconnected!", world.GetID()))
	w.Stop()

	wm.mu.Lock()
	defer wm.mu.Unlock()
	delete(wm.mapWorld, w.GetID())
	delete(wm.mapConn, con)
}

func (wm *WorldMgr) KickWorld(id uint32) {
	w, ok := wm.mapWorld[id]
	if !ok {
		return
	}

	if _, ok := wm.mapConn[w.GetCon()]; !ok {
		return
	}

	log.Warn(fmt.Sprintf("World<id:%d> was kicked by timeout reason!", world.GetID()))
	w.Stop()

	wm.mu.Lock()
	defer wm.mu.Unlock()
	delete(wm.mapConn, w.GetCon())
	delete(wm.mapWorld, w.GetID())
}

func (wm *WorldMgr) BroadCast(msg proto.Message) {
	for _, w := range wm.mapWorld {
		w.SendProtoMessage(msg)
	}
}

func (wm *WorldMgr) Run() {
	for {
		select {
		case <-wm.ctx.Done():
			log.Info("world session context done!")
			wm.chStop <- struct{}{}
			return
		case wid := <-wm.chTimeOutW:
			wm.KickWorld(wid)
		}
	}
}
