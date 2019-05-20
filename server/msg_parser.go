package server

import (
	"bytes"
	"encoding/binary"
	"errors"
	"fmt"
	"net"
	"reflect"
	"time"

	"github.com/golang/protobuf/proto"
	"github.com/hellodudu/Ultimate/logger"
	pb "github.com/hellodudu/Ultimate/proto"
	"github.com/hellodudu/Ultimate/utils"
)

// base net message type define
type baseNetMsg struct {
	Id   uint32 // message name crc32
	Size uint32 // message size
}

// transfer message type
type transferNetMsg struct {
	baseNetMsg
	WorldID  uint32 // world to recv message
	PlayerID int64  // player to recv message
}

// ProtoHandler handle function
type ProtoHandler func(net.Conn, proto.Message)

type MsgParser struct {
	protoHandler map[uint32]ProtoHandler
}

func NewMsgParser() *MsgHandle {
	m := &MsgParser{
		protoHandler: make(map[uint32]ProtoHandler),
	}

	m.registerAllMessage()
	return m
}

func (m *MsgParser) registerAllMessage() {
	m.regProtoHandle("world_message.MWU_WorldLogon", HandleWorldLogon)
	m.regProtoHandle("world_message.MWU_TestConnect", HandleTestConnect)
	m.regProtoHandle("world_message.MWU_HeartBeat", HandleHeartBeat)
	m.regProtoHandle("world_message.MWU_WorldConnected", HandleWorldConnected)
	m.regProtoHandle("world_message.MWU_RequestPlayerInfo", HandleRequestPlayerInfo)
	m.regProtoHandle("world_message.MWU_RequestGuildInfo", HandleRequestGuildInfo)
	m.regProtoHandle("world_message.MWU_PlayUltimateRecord", HandlePlayUltimateRecord)
	m.regProtoHandle("world_message.MWU_RequestUltimatePlayer", HandleRequestUltimatePlayer)
	m.regProtoHandle("world_message.MWU_ViewFormation", HandleViewFormation)
	m.regProtoHandle("world_message.MWU_ArenaMatching", HandleArenaMatching)
	m.regProtoHandle("world_message.MWU_ArenaAddRecord", HandleArenaAddRecord)
	m.regProtoHandle("world_message.MWU_ArenaBattleResult", HandleArenaBattleResult)
	m.regProtoHandle("world_message.MWU_ReplacePlayerInfo", HandleReplacePlayerInfo)
	m.regProtoHandle("world_message.MWU_ReplaceGuildInfo", HandleReplaceGuildInfo)
	m.regProtoHandle("world_message.MWU_RequestArenaRank", HandleRequestArenaRank)
	m.regProtoHandle("world_message.MWU_AddInvite", HandleAddInvite)
	m.regProtoHandle("world_message.MWU_CheckInviteResult", HandleCheckInviteResult)
	m.regProtoHandle("world_message.MWU_InviteRecharge", HandleInviteRecharge)
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

// top 8 bytes are baseNetMsg
// if it is protobuf msg, then next 2 bytes are proto name length, the next is proto name, final is proto data.
// if it is transfer msg(transfer binarys to other world), then next are binarys to be transferd
func (m *MsgParser) HandleMessage(con net.Conn, data []byte) {
	if len(data) <= 8 {
		logger.Warning("tcp recv data length <= 8:", string(data))
		return
	}

	baseMsg := &baseNetMsg{}
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
		newProto, err := m.decodeToProto(data)
		if err != nil {
			logger.Warning(err)
			return
		}

		protoMsgID := utils.Crc32(proto.MessageName(newProto))
		r, err := m.getRegProtoHandle(protoMsgID)
		if err != nil {
			logger.Warning(fmt.Sprintf("unregisted protoMsgID<%d> received!", protoMsgID))
		}

		// debug level <= 1 will not print log to screen
		if r.lv > 1 {
			logger.Info(fmt.Sprintf("recv world proto msg:%T", newProto))
		}

		// callback
		r.cb(con, m, newProto)

		// transfer message
	} else if baseMsg.Id == utils.Crc32(string("MWU_TransferMsg")) {
		transferMsg := &transferNetMsg{}
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
		sendWorld := m.GetWorldByID(transferMsg.WorldID)
		if sendWorld == nil {
			logger.Warning(fmt.Sprintf("send transfer message to unconnected world<%d>", transferMsg.WorldID))
			return
		}

		sendWorld.SendTransferMessage(data)
	}

}

func HandleWorldLogon(con net.Conn, m *MsgParser, p proto.Message) {
	msg, ok := p.(*pb.MWU_WorldLogon)
	if !ok {
		logger.Warning("Cannot assert value to message pb.MWU_WorldLogon")
		return
	}

	world, err := m.AddWorld(msg.WorldId, msg.WorldName, con)
	if err != nil {
		logger.Warning(err, fmt.Sprintf("<id:%d, name:%s, con:%v>", msg.WorldId, msg.WorldName, con))
		return
	}

	reply := &pb.MUW_WorldLogon{}
	world.SendProtoMessage(reply)

	// save to db
	fieldID, foundID := reflect.TypeOf(*world).FieldByName("Id")
	fieldName, foundName := reflect.TypeOf(*world).FieldByName("Name")
	if !foundID || !foundName {
		logger.Warning("cannot find world's field by Id or Name")
		return
	}

	query := fmt.Sprintf("replace into world(%s, %s, last_connect_time) values(%d, \"%s\", %d)", fieldID.Tag.Get("sql"), fieldName.Tag.Get("sql"), world.Id, world.Name, int32(time.Now().Unix()))

	Instance().dbMgr.Exec(query)
}

func HandleTestConnect(con net.Conn, m *MsgParser, p proto.Message) {
	if world := m.GetWorldByCon(con); world != nil {
		world.ResetTestConnect()
	}
}

func HandleHeartBeat(con net.Conn, m *MsgParser, p proto.Message) {
	if world := m.GetWorldByCon(con); world != nil {
		if t := int32(time.Now().Unix()); t == -1 {
			logger.Warning("Heart beat get time err")
			return
		}

		reply := &pb.MUW_HeartBeat{BattleTime: uint32(time.Now().Unix())}
		world.SendProtoMessage(reply)
	}
}

func HandleWorldConnected(con net.Conn, m *MsgParser, p proto.Message) {
	if world := m.GetWorldByCon(con); world != nil {
		arrWorldID := p.(*pb.MWU_WorldConnected).WorldId
		logger.Info(fmt.Sprintf("world ref<%v> connected!", arrWorldID))

		// add reference world id
		m.AddWorldRef(world.Id, arrWorldID)

		// request player info
		msgP := &pb.MUW_RequestPlayerInfo{MinLevel: 20}
		world.SendProtoMessage(msgP)

		// request guild info
		msgG := &pb.MUW_RequestGuildInfo{}
		world.SendProtoMessage(msgG)

		// sync arena data
		endTime := Instance().GetGameMgr().GetArena().GetSeasonEndTime()
		season := Instance().GetGameMgr().GetArena().GetSeason()
		msgArena := &pb.MUW_SyncArenaSeason{
			Season:  int32(season),
			EndTime: uint32(endTime),
		}
		world.SendProtoMessage(msgArena)

		// 20s later sync arena champion
		t := time.NewTimer(20 * time.Second)
		go func(id uint32) {
			<-t.C
			w := Instance().GetMsgParser().GetWorldByID(id)
			if w == nil {
				logger.Warning("world<", id, "> disconnected, cannot sync arena champion")
				return
			}

			msg := &pb.MUW_ArenaChampion{
				Data: Instance().GetGameMgr().GetArena().GetChampion(),
			}

			w.SendProtoMessage(msg)
			logger.Info("sync arena champion to world<id:", w.Id, ", name:", w.Name, ">")
		}(world.Id)
	}
}

func HandleRequestPlayerInfo(con net.Conn, m *MsgParser, p proto.Message) {
	if world := m.GetWorldByCon(con); world != nil {
		msg, ok := p.(*pb.MWU_RequestPlayerInfo)
		if !ok {
			logger.Warning("Cannot assert value to message pb.MWU_RequestPlayerInfo")
			return
		}

		Instance().GetGameMgr().AddPlayerInfoList(msg.Info)
	}
}

func HandleRequestGuildInfo(con net.Conn, m *MsgParser, p proto.Message) {
	if world := m.GetWorldByCon(con); world != nil {
		msg, ok := p.(*pb.MWU_RequestGuildInfo)
		if !ok {
			logger.Warning("Cannot assert value to message pb.MWU_RequestGuildInfo")
			return
		}

		Instance().GetGameMgr().AddGuildInfoList(msg.Info)
	}
}

func HandlePlayUltimateRecord(con net.Conn, m *MsgParser, p proto.Message) {
	if srcWorld := m.GetWorldByCon(con); srcWorld != nil {
		msg, ok := p.(*pb.MWU_PlayUltimateRecord)
		if !ok {
			logger.Warning("Cannot assert value to message pb.MWU_PlayUltimateRecord")
			return
		}

		dstWorld := m.GetWorldByID(msg.DstServerId)
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

func HandleRequestUltimatePlayer(con net.Conn, m *MsgParser, p proto.Message) {
	if srcWorld := m.GetWorldByCon(con); srcWorld != nil {
		msg, ok := p.(*pb.MWU_RequestUltimatePlayer)
		if !ok {
			logger.Warning("Cannot assert value to message pb.MWU_RequestUltimatePlayer")
			return
		}

		dstInfo := Instance().GetGameMgr().GetPlayerInfoByID(msg.DstPlayerId)
		dstWorld := m.GetWorldByID(msg.DstServerId)
		if dstInfo == nil {
			return
		}

		if int32(msg.DstServerId) == -1 {
			dstWorld = m.GetWorldByID(dstInfo.ServerId)
		}

		if dstWorld == nil {
			return
		}

		msgSend := &pb.MUW_RequestUltimatePlayer{
			SrcPlayerId: msg.SrcPlayerId,
			SrcServerId: msg.SrcServerId,
			DstPlayerId: msg.DstPlayerId,
			DstServerId: dstInfo.ServerId,
		}
		dstWorld.SendProtoMessage(msgSend)
	}
}

func HandleViewFormation(con net.Conn, m *MsgParser, p proto.Message) {
	if srcWorld := m.GetWorldByCon(con); srcWorld != nil {
		msg, ok := p.(*pb.MWU_ViewFormation)
		if !ok {
			logger.Warning("Cannot assert value to message pb.MWU_ViewFormation")
			return
		}

		dstInfo := Instance().GetGameMgr().GetPlayerInfoByID(msg.DstPlayerId)
		dstWorld := m.GetWorldByID(msg.DstServerId)
		if dstInfo == nil {
			return
		}

		if int32(msg.DstServerId) == -1 {
			dstWorld = m.GetWorldByID(dstInfo.ServerId)
		}

		if dstWorld == nil {
			return
		}

		msgSend := &pb.MUW_ViewFormation{
			SrcPlayerId: msg.SrcPlayerId,
			SrcServerId: msg.SrcServerId,
			DstPlayerId: msg.DstPlayerId,
			DstServerId: dstInfo.ServerId,
		}
		dstWorld.SendProtoMessage(msgSend)

	}
}

///////////////////////////////
// arena battle
//////////////////////////////
func HandleArenaMatching(con net.Conn, m *MsgParser, p proto.Message) {
	if srcWorld := m.GetWorldByCon(con); srcWorld != nil {
		msg, ok := p.(*pb.MWU_ArenaMatching)
		if !ok {
			logger.Warning("Cannot assert value to message pb.MWU_ArenaMatching")
			return
		}

		Instance().GetGameMgr().GetArena().Matching(msg.PlayerId)
	}
}

func HandleArenaAddRecord(con net.Conn, m *MsgParser, p proto.Message) {
	if srcWorld := m.GetWorldByCon(con); srcWorld != nil {
		msg, ok := p.(*pb.MWU_ArenaAddRecord)
		if !ok {
			logger.Warning("Cannot assert value to message pb.MWU_ArenaAddRecord")
			return
		}

		Instance().GetGameMgr().GetArena().AddRecord(msg.Record)
	}
}

func HandleArenaBattleResult(con net.Conn, m *MsgParser, p proto.Message) {
	if srcWorld := m.GetWorldByCon(con); srcWorld != nil {
		msg, ok := p.(*pb.MWU_ArenaBattleResult)
		if !ok {
			logger.Warning("Cannot assert value to message pb.MWU_ArenaBattleResult")
			return
		}

		Instance().GetGameMgr().GetArena().BattleResult(msg.AttackId, msg.TargetId, msg.AttackWin)
	}
}

func HandleReplacePlayerInfo(con net.Conn, m *MsgParser, p proto.Message) {
	if srcWorld := m.GetWorldByCon(con); srcWorld != nil {
		msg, ok := p.(*pb.MWU_ReplacePlayerInfo)
		if !ok {
			logger.Warning("Cannot assert value to message pb.MWU_ReplacePlayerInfo")
			return
		}

		Instance().GetGameMgr().AddPlayerInfo(msg.Info)
	}
}

func HandleReplaceGuildInfo(con net.Conn, m *MsgParser, p proto.Message) {
	if srcWorld := m.GetWorldByCon(con); srcWorld != nil {
		msg, ok := p.(*pb.MWU_ReplaceGuildInfo)
		if !ok {
			logger.Warning("Cannot assert value to message pb.MWU_ReplaceGuildInfo")
			return
		}

		Instance().GetGameMgr().AddGuildInfo(msg.Info)
	}
}

func HandleRequestArenaRank(con net.Conn, m *MsgParser, p proto.Message) {
	if srcWorld := m.GetWorldByCon(con); srcWorld != nil {
		msg, ok := p.(*pb.MWU_RequestArenaRank)
		if !ok {
			logger.Warning("Cannot assert value to message pb.MWU_RequestArenaRank")
			return
		}

		Instance().GetGameMgr().GetArena().RequestRank(msg.PlayerId, msg.Page)
	}
}

func HandleAddInvite(con net.Conn, m *MsgParser, p proto.Message) {
	if srcWorld := m.GetWorldByCon(con); srcWorld != nil {
		msg, ok := p.(*pb.MWU_AddInvite)
		if !ok {
			logger.Warning("Cannot assert value to message pb.MWU_AddInvite")
			return
		}

		Instance().GetGameMgr().GetInvite().AddInvite(msg.NewbieId, msg.InviterId)
	}
}

func HandleCheckInviteResult(con net.Conn, m *MsgParser, p proto.Message) {
	if srcWorld := m.GetWorldByCon(con); srcWorld != nil {
		msg, ok := p.(*pb.MWU_CheckInviteResult)
		if !ok {
			logger.Warning("Cannot assert value to message pb.MWU_CheckInviteResult")
			return
		}

		Instance().GetGameMgr().GetInvite().CheckInviteResult(msg.NewbieId, msg.InviterId, msg.ErrorCode)
	}
}

func HandleInviteRecharge(con net.Conn, m *MsgParser, p proto.Message) {
	if srcWorld := m.GetWorldByCon(con); srcWorld != nil {
		msg, ok := p.(*pb.MWU_InviteRecharge)
		if !ok {
			logger.Warning("Cannot assert value to message pb.MWU_InviteRecharge")
			return
		}

		Instance().GetGameMgr().GetInvite().InviteRecharge(msg.NewbieId, msg.NewbieName, msg.InviterId, msg.DiamondGift)
	}
}
