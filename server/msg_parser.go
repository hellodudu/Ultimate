package server

import (
	"bytes"
	"encoding/binary"
	"errors"
	"fmt"
	"reflect"
	"time"

	"github.com/golang/protobuf/proto"
	"github.com/hellodudu/Ultimate/global"
	"github.com/hellodudu/Ultimate/iface"
	"github.com/hellodudu/Ultimate/logger"
	pb "github.com/hellodudu/Ultimate/proto"
	"github.com/hellodudu/Ultimate/utils"
)

// ProtoHandler handle function
type ProtoHandler func(iface.ITCPConn, proto.Message)

type MsgParser struct {
	protoHandler map[uint32]ProtoHandler
	gm           iface.IGameMgr
	wm           iface.IWorldMgr
}

func NewMsgParser(gm iface.IGameMgr, wm iface.IWorldMgr) *MsgParser {
	m := &MsgParser{
		protoHandler: make(map[uint32]ProtoHandler),
		gm:           gm,
		wm:           wm,
	}

	m.registerAllMessage()
	return m
}

func (m *MsgParser) registerAllMessage() {
	m.regProtoHandle("world_message.MWU_WorldLogon", m.handleWorldLogon)
	m.regProtoHandle("world_message.MWU_TestConnect", m.handleTestConnect)
	m.regProtoHandle("world_message.MWU_HeartBeat", m.handleHeartBeat)
	m.regProtoHandle("world_message.MWU_WorldConnected", m.handleWorldConnected)
	m.regProtoHandle("world_message.MWU_RequestPlayerInfo", m.handleRequestPlayerInfo)
	m.regProtoHandle("world_message.MWU_RequestGuildInfo", m.handleRequestGuildInfo)
	m.regProtoHandle("world_message.MWU_PlayUltimateRecord", m.handlePlayUltimateRecord)
	m.regProtoHandle("world_message.MWU_RequestUltimatePlayer", m.handleRequestUltimatePlayer)
	m.regProtoHandle("world_message.MWU_ViewFormation", m.handleViewFormation)
	m.regProtoHandle("world_message.MWU_ArenaMatching", m.handleArenaMatching)
	m.regProtoHandle("world_message.MWU_ArenaAddRecord", m.handleArenaAddRecord)
	m.regProtoHandle("world_message.MWU_ArenaBattleResult", m.handleArenaBattleResult)
	m.regProtoHandle("world_message.MWU_ReplacePlayerInfo", m.handleReplacePlayerInfo)
	m.regProtoHandle("world_message.MWU_ReplaceGuildInfo", m.handleReplaceGuildInfo)
	m.regProtoHandle("world_message.MWU_RequestArenaRank", m.handleRequestArenaRank)
	m.regProtoHandle("world_message.MWU_AddInvite", m.handleAddInvite)
	m.regProtoHandle("world_message.MWU_CheckInviteResult", m.handleCheckInviteResult)
	m.regProtoHandle("world_message.MWU_InviteRecharge", m.handleInviteRecharge)
	m.regProtoHandle("world_message.MWU_ArenaChampionOnline", m.handleArenaChampionOnline)
	m.regProtoHandle("world_message.MWU_SyncArenaSeason", m.handlerArenaSyncSeason)

}

func (m *MsgParser) getRegProtoHandle(id uint32) (ProtoHandler, error) {
	v, ok := m.protoHandler[id]
	if ok {
		return v, nil
	}

	return nil, errors.New("cannot find proto type registed in msg_handle!")
}

func (m *MsgParser) regProtoHandle(name string, fn ProtoHandler) {
	id := utils.Crc32(name)
	if v, ok := m.protoHandler[id]; ok {
		logger.Warning(fmt.Sprintf("register proto msg_id<%d> existed! protobuf type:%v\n", id, v))
		return
	}

	m.protoHandler[id] = fn
}

// decode binarys to proto message
func (m *MsgParser) decodeToProto(data []byte) (proto.Message, error) {

	// discard top 8 bytes of message size and message crc id
	byProto := data[8:]

	// get next 2 bytes of message name length
	protoNameLen := binary.LittleEndian.Uint16(byProto[:2])

	if uint16(len(byProto)) < 2+protoNameLen {
		return nil, fmt.Errorf("recv proto msg length<%d> less than 2+protoNameLen<%d>, with byte data:", uint16(len(byProto)), protoNameLen, byProto)
	}

	// get proto name
	protoTypeName := string(byProto[2 : 2+protoNameLen])
	pType := proto.MessageType(protoTypeName)
	if pType == nil {
		return nil, fmt.Errorf("invalid message<%s>, won't deal with it", protoTypeName)
	}

	// get proto data
	protoData := byProto[2+protoNameLen:]

	// prepare proto struct to be unmarshaled in
	newProto, ok := reflect.New(pType.Elem()).Interface().(proto.Message)
	if !ok {
		return nil, fmt.Errorf("invalid message<%s>, won't deal with it", protoTypeName)
	}

	// unmarshal
	if err := proto.Unmarshal(protoData, newProto); err != nil {
		logger.Warning("Failed to parse proto msg:", newProto, err)
		return nil, fmt.Errorf("invalid message<%s>, won't deal with it", protoTypeName)
	}

	return newProto, nil
}

// top 8 bytes are baseNetMsg
// if it is protobuf msg, then next 2 bytes are proto name length, the next is proto name, final is proto data.
// if it is transfer msg(transfer binarys to other world), then next are binarys to be transferd
func (m *MsgParser) ParserMessage(con iface.ITCPConn, data []byte) {
	if len(data) <= 8 {
		logger.Warning("tcp recv data length <= 8:", string(data))
		return
	}

	baseMsg := &global.BaseNetMsg{}
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
	if baseMsg.ID == utils.Crc32(string("MWU_DirectProtoMsg")) {
		newProto, err := m.decodeToProto(data)
		if err != nil {
			logger.Warning(err)
			return
		}

		protoMsgID := utils.Crc32(proto.MessageName(newProto))
		fn, err := m.getRegProtoHandle(protoMsgID)
		if err != nil {
			logger.Warning(fmt.Sprintf("unregisted protoMsgID<%d> received!", protoMsgID))
		}

		// callback
		fn(con, newProto)

		// transfer message
	} else if baseMsg.ID == utils.Crc32(string("MWU_TransferMsg")) {
		transferMsg := &global.TransferNetMsg{}
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

		// send message to world
		sendWorld := m.wm.GetWorldByID(transferMsg.WorldID)
		if sendWorld == nil {
			logger.Warning(fmt.Sprintf("send transfer message to unconnected world<%d>", transferMsg.WorldID))
			return
		}

		sendWorld.SendTransferMessage(data)
	}

}

func (m *MsgParser) handleWorldLogon(con iface.ITCPConn, p proto.Message) {
	msg, ok := p.(*pb.MWU_WorldLogon)
	if !ok {
		logger.Warning("Cannot assert value to message")
		return
	}

	world, err := m.wm.AddWorld(msg.WorldId, msg.WorldName, con)
	if err != nil {
		logger.Warning(err, fmt.Sprintf("<id:%d, name:%s, con:%v>", msg.WorldId, msg.WorldName, con))
		return
	}

	reply := &pb.MUW_WorldLogon{}
	world.SendProtoMessage(reply)

}

func (m *MsgParser) handleTestConnect(con iface.ITCPConn, p proto.Message) {
	if world := m.wm.GetWorldByCon(con); world != nil {
		world.ResetTestConnect()
	}
}

func (m *MsgParser) handleHeartBeat(con iface.ITCPConn, p proto.Message) {
	if world := m.wm.GetWorldByCon(con); world != nil {
		if t := int32(time.Now().Unix()); t == -1 {
			logger.Warning("Heart beat get time err")
			return
		}

		reply := &pb.MUW_HeartBeat{BattleTime: uint32(time.Now().Unix())}
		world.SendProtoMessage(reply)
	}
}

func (m *MsgParser) handleWorldConnected(con iface.ITCPConn, p proto.Message) {
	if world := m.wm.GetWorldByCon(con); world != nil {
		arrWorldID := p.(*pb.MWU_WorldConnected).WorldId
		logger.Info(fmt.Sprintf("world ref<%v> connected!", arrWorldID))

		// add reference world id
		m.wm.AddWorldRef(world.GetID(), arrWorldID)

		// request player info
		msgP := &pb.MUW_RequestPlayerInfo{MinLevel: 20}
		world.SendProtoMessage(msgP)

		// request guild info
		msgG := &pb.MUW_RequestGuildInfo{}
		world.SendProtoMessage(msgG)

		// sync arena data
		endTime := m.gm.Arena().SeasonEndTime()
		season := m.gm.Arena().Season()
		msgArena := &pb.MUW_SyncArenaSeason{
			Season:  int32(season),
			EndTime: uint32(endTime),
		}
		world.SendProtoMessage(msgArena)

		// 20s later sync arena champion
		t := time.NewTimer(20 * time.Second)
		go func(id uint32) {
			<-t.C
			w := m.wm.GetWorldByID(id)
			if w == nil {
				logger.Warning("world<", id, "> disconnected, cannot sync arena champion")
				return
			}

			msg := &pb.MUW_ArenaChampion{
				Data: m.gm.Arena().GetChampion(),
			}

			w.SendProtoMessage(msg)
			logger.Info("sync arena champion to world<id:", w.GetID(), ", name:", w.GetName(), ">")
		}(world.GetID())
	}
}

func (m *MsgParser) handleRequestPlayerInfo(con iface.ITCPConn, p proto.Message) {
	if world := m.wm.GetWorldByCon(con); world != nil {
		msg, ok := p.(*pb.MWU_RequestPlayerInfo)
		if !ok {
			logger.Warning("Cannot assert value to message pb.MWU_RequestPlayerInfo")
			return
		}

		m.gm.AddPlayerInfoList(msg.Info)
	}
}

func (m *MsgParser) handleRequestGuildInfo(con iface.ITCPConn, p proto.Message) {
	if world := m.wm.GetWorldByCon(con); world != nil {
		msg, ok := p.(*pb.MWU_RequestGuildInfo)
		if !ok {
			logger.Warning("Cannot assert value to message pb.MWU_RequestGuildInfo")
			return
		}

		m.gm.AddGuildInfoList(msg.Info)
	}
}

func (m *MsgParser) handlePlayUltimateRecord(con iface.ITCPConn, p proto.Message) {
	if srcWorld := m.wm.GetWorldByCon(con); srcWorld != nil {
		msg, ok := p.(*pb.MWU_PlayUltimateRecord)
		if !ok {
			logger.Warning("Cannot assert value to message pb.MWU_PlayUltimateRecord")
			return
		}

		dstWorld := m.wm.GetWorldByID(msg.DstServerId)
		if dstWorld == nil {
			return
		}

		msgSend := &pb.MUW_PlayUltimateRecord{
			SrcPlayerId: msg.SrcPlayerId,
			SrcServerId: msg.SrcServerId,
			RecordId:    msg.RecordId,
			DstServerId: msg.DstServerId,
		}
		dstWorld.SendProtoMessage(msgSend)
	}
}

func (m *MsgParser) handleRequestUltimatePlayer(con iface.ITCPConn, p proto.Message) {
	if srcWorld := m.wm.GetWorldByCon(con); srcWorld != nil {
		msg, ok := p.(*pb.MWU_RequestUltimatePlayer)
		if !ok {
			logger.Warning("Cannot assert value to message pb.MWU_RequestUltimatePlayer")
			return
		}

		dstInfo := m.gm.GetPlayerInfoByID(msg.DstPlayerId)
		dstWorld := m.wm.GetWorldByID(msg.DstServerId)
		if dstInfo == nil {
			return
		}

		if int32(msg.DstServerId) == -1 {
			dstWorld = m.wm.GetWorldByID(dstInfo.ServerId)
		}

		if dstWorld == nil {
			return
		}

		msgSend := &pb.MUW_RequestUltimatePlayer{
			SrcPlayerId: msg.SrcPlayerId,
			SrcServerId: msg.SrcServerId,
			DstPlayerId: msg.DstPlayerId,
			DstServerId: dstWorld.GetID(),
		}
		dstWorld.SendProtoMessage(msgSend)
	}
}

func (m *MsgParser) handleViewFormation(con iface.ITCPConn, p proto.Message) {
	if srcWorld := m.wm.GetWorldByCon(con); srcWorld != nil {
		msg, ok := p.(*pb.MWU_ViewFormation)
		if !ok {
			logger.Warning("Cannot assert value to message pb.MWU_ViewFormation")
			return
		}

		dstInfo := m.gm.GetPlayerInfoByID(msg.DstPlayerId)
		dstWorld := m.wm.GetWorldByID(msg.DstServerId)
		if dstInfo == nil {
			return
		}

		if int32(msg.DstServerId) == -1 {
			dstWorld = m.wm.GetWorldByID(dstInfo.ServerId)
		}

		if dstWorld == nil {
			return
		}

		msgSend := &pb.MUW_ViewFormation{
			SrcPlayerId: msg.SrcPlayerId,
			SrcServerId: msg.SrcServerId,
			DstPlayerId: msg.DstPlayerId,
			DstServerId: dstWorld.GetID(),
		}
		dstWorld.SendProtoMessage(msgSend)
	}
}

///////////////////////////////
// arena battle
//////////////////////////////
func (m *MsgParser) handleArenaMatching(con iface.ITCPConn, p proto.Message) {
	if srcWorld := m.wm.GetWorldByCon(con); srcWorld != nil {
		msg, ok := p.(*pb.MWU_ArenaMatching)
		if !ok {
			logger.Warning("Cannot assert value to message pb.MWU_ArenaMatching")
			return
		}

		m.gm.Arena().Matching(msg.PlayerId)
	}
}

func (m *MsgParser) handleArenaAddRecord(con iface.ITCPConn, p proto.Message) {
	if srcWorld := m.wm.GetWorldByCon(con); srcWorld != nil {
		msg, ok := p.(*pb.MWU_ArenaAddRecord)
		if !ok {
			logger.Warning("Cannot assert value to message pb.MWU_ArenaAddRecord")
			return
		}

		m.gm.Arena().AddRecord(msg.Record)
	}
}

func (m *MsgParser) handleArenaBattleResult(con iface.ITCPConn, p proto.Message) {
	if srcWorld := m.wm.GetWorldByCon(con); srcWorld != nil {
		msg, ok := p.(*pb.MWU_ArenaBattleResult)
		if !ok {
			logger.Warning("Cannot assert value to message pb.MWU_ArenaBattleResult")
			return
		}

		m.gm.Arena().BattleResult(msg.AttackId, msg.TargetId, msg.AttackWin)
	}
}

func (m *MsgParser) handleReplacePlayerInfo(con iface.ITCPConn, p proto.Message) {
	if srcWorld := m.wm.GetWorldByCon(con); srcWorld != nil {
		msg, ok := p.(*pb.MWU_ReplacePlayerInfo)
		if !ok {
			logger.Warning("Cannot assert value to message pb.MWU_ReplacePlayerInfo")
			return
		}

		m.gm.AddPlayerInfo(msg.Info)
	}
}

func (m *MsgParser) handleReplaceGuildInfo(con iface.ITCPConn, p proto.Message) {
	if srcWorld := m.wm.GetWorldByCon(con); srcWorld != nil {
		msg, ok := p.(*pb.MWU_ReplaceGuildInfo)
		if !ok {
			logger.Warning("Cannot assert value to message pb.MWU_ReplaceGuildInfo")
			return
		}

		m.gm.AddGuildInfo(msg.Info)
	}
}

func (m *MsgParser) handleRequestArenaRank(con iface.ITCPConn, p proto.Message) {
	if srcWorld := m.wm.GetWorldByCon(con); srcWorld != nil {
		msg, ok := p.(*pb.MWU_RequestArenaRank)
		if !ok {
			logger.Warning("Cannot assert value to message pb.MWU_RequestArenaRank")
			return
		}

		m.gm.Arena().RequestRank(msg.PlayerId, msg.Page)
	}
}

func (m *MsgParser) handleAddInvite(con iface.ITCPConn, p proto.Message) {
	if srcWorld := m.wm.GetWorldByCon(con); srcWorld != nil {
		msg, ok := p.(*pb.MWU_AddInvite)
		if !ok {
			logger.Warning("Cannot assert value to message pb.MWU_AddInvite")
			return
		}

		ret := m.gm.Invite().AddInvite(msg.NewbieId, msg.InviterId)
		if ret != 0 {
			msgRet := &pb.MUW_AddInviteResult{
				NewbieId:  msg.NewbieId,
				InviterId: msg.InviterId,
				ErrorCode: ret,
			}

			srcWorld.SendProtoMessage(msgRet)
		}
	}
}

func (m *MsgParser) handleCheckInviteResult(con iface.ITCPConn, p proto.Message) {
	if srcWorld := m.wm.GetWorldByCon(con); srcWorld != nil {
		msg, ok := p.(*pb.MWU_CheckInviteResult)
		if !ok {
			logger.Warning("Cannot assert value to message pb.MWU_CheckInviteResult")
			return
		}

		m.gm.Invite().CheckInviteResult(msg.NewbieId, msg.InviterId, msg.ErrorCode)
	}
}

func (m *MsgParser) handleInviteRecharge(con iface.ITCPConn, p proto.Message) {
	if srcWorld := m.wm.GetWorldByCon(con); srcWorld != nil {
		msg, ok := p.(*pb.MWU_InviteRecharge)
		if !ok {
			logger.Warning("Cannot assert value to message pb.MWU_InviteRecharge")
			return
		}

		m.gm.Invite().InviteRecharge(msg.NewbieId, msg.NewbieName, msg.InviterId, msg.DiamondGift)
	}
}

func (m *MsgParser) handleArenaChampionOnline(con iface.ITCPConn, p proto.Message) {
	if srcWorld := m.wm.GetWorldByCon(con); srcWorld != nil {
		msg, ok := p.(*pb.MWU_ArenaChampionOnline)
		if !ok {
			logger.Warning("Cannot assert value to message pb.MWU_ArenaChampionOnline")
			return
		}

		msgSend := &pb.MUW_ArenaChampionOnline{
			PlayerId:   msg.PlayerId,
			PlayerName: msg.PlayerName,
			ServerName: msg.ServerName,
		}

		m.wm.BroadCast(msgSend)
	}
}

func (m *MsgParser) handlerArenaSyncSeason(con iface.ITCPConn, p proto.Message) {
	if srcWorld := m.wm.GetWorldByCon(con); srcWorld != nil {
		_, ok := p.(*pb.MWU_SyncArenaSeason)
		if !ok {
			logger.Warning("Cannot assert value to message pb.MWU_SyncArenaSeason")
			return
		}

		m.gm.Arena().SyncArenaSeason(srcWorld.GetID())
	}
}
