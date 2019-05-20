package world

import (
	"bytes"
	"context"
	"encoding/binary"
	"errors"
	"fmt"
	"net"
	"reflect"
	"sync"

	"github.com/golang/protobuf/proto"
	"github.com/hellodudu/Ultimate/global"
	"github.com/hellodudu/Ultimate/logger"
	"github.com/hellodudu/Ultimate/utils"
)

// base net message type define
type BaseNetMsg struct {
	Id   uint32 // message name crc32
	Size uint32 // message size
}

// transfer message type
type TransferNetMsg struct {
	BaseNetMsg
	WorldID  uint32 // world to recv message
	PlayerID int64  // player to recv message
}

// world session msg register info
type regInfo struct {
	lv int // debug level
	cb func(net.Conn, *WorldMgr, proto.Message)
}

type WorldMgr struct {
	mapWorld      sync.Map
	mapConn       sync.Map
	mapRefWorldID sync.Map
	protoReg      map[uint32]*regInfo
	wg            sync.WaitGroup
	ctx           context.Context
	cancel        context.CancelFunc
	chTimeOutW    chan uint32
	chStop        chan struct{}
}

func NewWorldMgr() (*WorldMgr, error) {
	w := &WorldMgr{
		protoReg:   make(map[uint32]*regInfo),
		chTimeOutW: make(chan uint32, global.WorldConnectMax),
		chStop:     make(chan struct{}, 1),
	}

	w.ctx, w.cancel = context.WithCancel(context.Background())

	w.registerAllMessage()
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

func (ws *WorldMgr) registerAllMessage() {
	// cb -- callback function
	ws.registerProto(utils.Crc32("world_message.MWU_WorldLogon"), &regInfo{
		lv: 1,
		cb: HandleWorldLogon,
	})

	ws.registerProto(utils.Crc32("world_message.MWU_TestConnect"), &regInfo{
		lv: 0,
		cb: HandleTestConnect,
	})

	ws.registerProto(utils.Crc32("world_message.MWU_HeartBeat"), &regInfo{
		lv: 0,
		cb: HandleHeartBeat,
	})

	ws.registerProto(utils.Crc32("world_message.MWU_WorldConnected"), &regInfo{
		lv: 2,
		cb: HandleWorldConnected,
	})

	ws.registerProto(utils.Crc32("world_message.MWU_RequestPlayerInfo"), &regInfo{
		lv: 1,
		cb: HandleRequestPlayerInfo,
	})

	ws.registerProto(utils.Crc32("world_message.MWU_RequestGuildInfo"), &regInfo{
		lv: 1,
		cb: HandleRequestGuildInfo,
	})

	ws.registerProto(utils.Crc32("world_message.MWU_PlayUltimateRecord"), &regInfo{
		lv: 1,
		cb: HandlePlayUltimateRecord,
	})

	ws.registerProto(utils.Crc32("world_message.MWU_RequestUltimatePlayer"), &regInfo{
		lv: 1,
		cb: HandleRequestUltimatePlayer,
	})

	ws.registerProto(utils.Crc32("world_message.MWU_ViewFormation"), &regInfo{
		lv: 1,
		cb: HandleViewFormation,
	})

	ws.registerProto(utils.Crc32("world_message.MWU_ArenaMatching"), &regInfo{
		lv: 1,
		cb: HandleArenaMatching,
	})

	ws.registerProto(utils.Crc32("world_message.MWU_ArenaAddRecord"), &regInfo{
		lv: 1,
		cb: HandleArenaAddRecord,
	})

	ws.registerProto(utils.Crc32("world_message.MWU_ArenaBattleResult"), &regInfo{
		lv: 1,
		cb: HandleArenaBattleResult,
	})

	ws.registerProto(utils.Crc32("world_message.MWU_ReplacePlayerInfo"), &regInfo{
		lv: 1,
		cb: HandleReplacePlayerInfo,
	})

	ws.registerProto(utils.Crc32("world_message.MWU_ReplaceGuildInfo"), &regInfo{
		lv: 1,
		cb: HandleReplaceGuildInfo,
	})

	ws.registerProto(utils.Crc32("world_message.MWU_RequestArenaRank"), &regInfo{
		lv: 1,
		cb: HandleRequestArenaRank,
	})

	ws.registerProto(utils.Crc32("world_message.MWU_AddInvite"), &regInfo{
		lv: 1,
		cb: HandleAddInvite,
	})

	ws.registerProto(utils.Crc32("world_message.MWU_CheckInviteResult"), &regInfo{
		lv: 1,
		cb: HandleCheckInviteResult,
	})

	ws.registerProto(utils.Crc32("world_message.MWU_InviteRecharge"), &regInfo{
		lv: 1,
		cb: HandleInviteRecharge,
	})
}

func (ws *WorldMgr) getRegisterProto(msgID uint32) (*regInfo, error) {
	v, ok := ws.protoReg[msgID]
	if ok {
		return v, nil
	}

	return v, errors.New("cannot find proto type registed in world_session!")
}

func (ws *WorldMgr) registerProto(msgID uint32, info *regInfo) {
	if v, ok := ws.protoReg[msgID]; ok {
		logger.Warning(fmt.Sprintf("register proto msg_id<%d> existed! protobuf type:%v\n", msgID, v))
		return
	}

	ws.protoReg[msgID] = info
}

// decode binarys to proto message
func (ws *WorldMgr) decodeToProto(data []byte) (proto.Message, error) {
	byProto := data[8:]
	protoNameLen := binary.LittleEndian.Uint16(byProto[:2])

	if uint16(len(byProto)) < 2+protoNameLen {
		return nil, fmt.Errorf("recv proto msg length < 2+protoNameLen:" + string(byProto))
	}

	protoTypeName := string(byProto[2 : 2+protoNameLen])
	protoData := byProto[2+protoNameLen:]
	pType := proto.MessageType(protoTypeName)
	if pType == nil {
		return nil, fmt.Errorf("invalid message<%s>, won't deal with it", protoTypeName)
	}

	// unmarshal
	newProto, ok := reflect.New(pType.Elem()).Interface().(proto.Message)
	if !ok {
		return nil, fmt.Errorf("invalid message<%s>, won't deal with it", protoTypeName)
	}

	if err := proto.Unmarshal(protoData, newProto); err != nil {
		logger.Warning("Failed to parse proto msg:", newProto, err)
		return nil, fmt.Errorf("invalid message<%s>, won't deal with it", protoTypeName)
	}

	return newProto, nil
}

// top 8 bytes are BaseNetMsg
// if it is protobuf msg, then next 2 bytes are proto name length, the next is proto name, final is proto data.
// if it is transfer msg(transfer binarys to other world), then next are binarys to be transferd
func (ws *WorldMgr) HandleMessage(con net.Conn, data []byte) {
	if len(data) <= 8 {
		logger.Warning("tcp recv data length <= 8:", string(data))
		return
	}

	baseMsg := &BaseNetMsg{}
	byBaseMsg := make([]byte, binary.Size(baseMsg))

	copy(byBaseMsg, data[:binary.Size(baseMsg)])
	buf := &bytes.Buffer{}
	if _, err := buf.Write(byBaseMsg); err != nil {
		logger.Warning("cannot read message:", byBaseMsg, " from connection:", con, " err:", err)
		return
	}

	// get top 4 bytes messageid
	if err := binary.Read(buf, binary.LittleEndian, baseMsg); err != nil {
		logger.Warning("cannot read message:", byBaseMsg, " from connection:", con, " err:", err)
		return
	}

	// proto message
	if baseMsg.Id == utils.Crc32(string("MWU_DirectProtoMsg")) {
		newProto, err := ws.decodeToProto(data)
		if err != nil {
			logger.Warning(err)
			return
		}

		protoMsgID := utils.Crc32(proto.MessageName(newProto))
		r, err := ws.getRegisterProto(protoMsgID)
		if err != nil {
			logger.Warning(fmt.Sprintf("unregisted protoMsgID<%d> received!", protoMsgID))
		}

		// debug level <= 1 will not print log to screen
		if r.lv > 1 {
			logger.Info(fmt.Sprintf("recv world proto msg:%T", newProto))
		}

		// callback
		r.cb(con, ws, newProto)

		// transfer message
	} else if baseMsg.Id == utils.Crc32(string("MWU_TransferMsg")) {
		transferMsg := &TransferNetMsg{}
		byTransferMsg := make([]byte, binary.Size(transferMsg))

		copy(byTransferMsg, data[:binary.Size(transferMsg)])
		buf := &bytes.Buffer{}
		if _, err := buf.Write(byTransferMsg); err != nil {
			logger.Warning("cannot read message:", byTransferMsg, " from connection:", con, " err:", err)
			return
		}

		// get top 4 bytes messageid
		if err := binary.Read(buf, binary.LittleEndian, transferMsg); err != nil {
			logger.Warning("cannot read message:", byTransferMsg, " from connection:", con, " err:", err)
			return
		}

		logger.Info(fmt.Sprintf("recv transfer msg to world<%d> player<%d>", transferMsg.WorldID, transferMsg.PlayerID))

		// send message to world
		sendWorld := ws.GetWorldByID(transferMsg.WorldID)
		if sendWorld == nil {
			logger.Warning(fmt.Sprintf("send transfer message to unconnected world<%d>", transferMsg.WorldID))
			return
		}

		sendWorld.SendTransferMessage(data)
	}

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
