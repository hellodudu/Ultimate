package server

import (
	"fmt"
	"net"
	"reflect"
	"time"

	"github.com/golang/protobuf/proto"
	"github.com/hellodudu/Ultimate/logger"
	world_message "github.com/hellodudu/Ultimate/proto"
)

func HandleWorldLogon(con net.Conn, ws *WorldMgr, p proto.Message) {
	msg, ok := p.(*world_message.MWU_WorldLogon)
	if !ok {
		logger.Warning("Cannot assert value to message world_message.MWU_WorldLogon")
		return
	}

	world, err := ws.AddWorld(msg.WorldId, msg.WorldName, con)
	if err != nil {
		logger.Warning(err, fmt.Sprintf("<id:%d, name:%s, con:%v>", msg.WorldId, msg.WorldName, con))
		return
	}

	reply := &world_message.MUW_WorldLogon{}
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

func HandleTestConnect(con net.Conn, ws *WorldMgr, p proto.Message) {
	if world := ws.GetWorldByCon(con); world != nil {
		world.ResetTestConnect()
	}
}

func HandleHeartBeat(con net.Conn, ws *WorldMgr, p proto.Message) {
	if world := ws.GetWorldByCon(con); world != nil {
		if t := int32(time.Now().Unix()); t == -1 {
			logger.Warning("Heart beat get time err")
			return
		}

		reply := &world_message.MUW_HeartBeat{BattleTime: uint32(time.Now().Unix())}
		world.SendProtoMessage(reply)
	}
}

func HandleWorldConnected(con net.Conn, ws *WorldMgr, p proto.Message) {
	if world := ws.GetWorldByCon(con); world != nil {
		arrWorldID := p.(*world_message.MWU_WorldConnected).WorldId
		logger.Info(fmt.Sprintf("world ref<%v> connected!", arrWorldID))

		// add reference world id
		ws.AddWorldRef(world.Id, arrWorldID)

		// request player info
		msgP := &world_message.MUW_RequestPlayerInfo{MinLevel: 20}
		world.SendProtoMessage(msgP)

		// request guild info
		msgG := &world_message.MUW_RequestGuildInfo{}
		world.SendProtoMessage(msgG)

		// sync arena data
		endTime := Instance().GetGameMgr().GetArena().GetSeasonEndTime()
		season := Instance().GetGameMgr().GetArena().GetSeason()
		msgArena := &world_message.MUW_SyncArenaSeason{
			Season:  int32(season),
			EndTime: uint32(endTime),
		}
		world.SendProtoMessage(msgArena)

		// 20s later sync arena champion
		t := time.NewTimer(20 * time.Second)
		go func(id uint32) {
			<-t.C
			w := Instance().GetWorldMgr().GetWorldByID(id)
			if w == nil {
				logger.Warning("world<", id, "> disconnected, cannot sync arena champion")
				return
			}

			msg := &world_message.MUW_ArenaChampion{
				Data: Instance().GetGameMgr().GetArena().GetChampion(),
			}

			w.SendProtoMessage(msg)
			logger.Info("sync arena champion to world<id:", w.Id, ", name:", w.Name, ">")
		}(world.Id)
	}
}

func HandleRequestPlayerInfo(con net.Conn, ws *WorldMgr, p proto.Message) {
	if world := ws.GetWorldByCon(con); world != nil {
		msg, ok := p.(*world_message.MWU_RequestPlayerInfo)
		if !ok {
			logger.Warning("Cannot assert value to message world_message.MWU_RequestPlayerInfo")
			return
		}

		Instance().GetGameMgr().AddPlayerInfoList(msg.Info)
	}
}

func HandleRequestGuildInfo(con net.Conn, ws *WorldMgr, p proto.Message) {
	if world := ws.GetWorldByCon(con); world != nil {
		msg, ok := p.(*world_message.MWU_RequestGuildInfo)
		if !ok {
			logger.Warning("Cannot assert value to message world_message.MWU_RequestGuildInfo")
			return
		}

		Instance().GetGameMgr().AddGuildInfoList(msg.Info)
	}
}

func HandlePlayUltimateRecord(con net.Conn, ws *WorldMgr, p proto.Message) {
	if srcWorld := ws.GetWorldByCon(con); srcWorld != nil {
		msg, ok := p.(*world_message.MWU_PlayUltimateRecord)
		if !ok {
			logger.Warning("Cannot assert value to message world_message.MWU_PlayUltimateRecord")
			return
		}

		dstWorld := ws.GetWorldByID(msg.DstServerId)
		if dstWorld == nil {
			return
		}

		msgSend := &world_message.MUW_PlayUltimateRecord{
			SrcPlayerId: msg.SrcPlayerId,
			SrcServerId: msg.SrcServerId,
			RecordId:    msg.RecordId,
			DstServerId: msg.DstServerId,
		}
		dstWorld.SendProtoMessage(msgSend)
	}
}

func HandleRequestUltimatePlayer(con net.Conn, ws *WorldMgr, p proto.Message) {
	if srcWorld := ws.GetWorldByCon(con); srcWorld != nil {
		msg, ok := p.(*world_message.MWU_RequestUltimatePlayer)
		if !ok {
			logger.Warning("Cannot assert value to message world_message.MWU_RequestUltimatePlayer")
			return
		}

		dstInfo := Instance().GetGameMgr().GetPlayerInfoByID(msg.DstPlayerId)
		dstWorld := ws.GetWorldByID(msg.DstServerId)
		if dstInfo == nil {
			return
		}

		if int32(msg.DstServerId) == -1 {
			dstWorld = ws.GetWorldByID(dstInfo.ServerId)
		}

		if dstWorld == nil {
			return
		}

		msgSend := &world_message.MUW_RequestUltimatePlayer{
			SrcPlayerId: msg.SrcPlayerId,
			SrcServerId: msg.SrcServerId,
			DstPlayerId: msg.DstPlayerId,
			DstServerId: dstInfo.ServerId,
		}
		dstWorld.SendProtoMessage(msgSend)
	}
}

func HandleViewFormation(con net.Conn, ws *WorldMgr, p proto.Message) {
	if srcWorld := ws.GetWorldByCon(con); srcWorld != nil {
		msg, ok := p.(*world_message.MWU_ViewFormation)
		if !ok {
			logger.Warning("Cannot assert value to message world_message.MWU_ViewFormation")
			return
		}

		dstInfo := Instance().GetGameMgr().GetPlayerInfoByID(msg.DstPlayerId)
		dstWorld := ws.GetWorldByID(msg.DstServerId)
		if dstInfo == nil {
			return
		}

		if int32(msg.DstServerId) == -1 {
			dstWorld = ws.GetWorldByID(dstInfo.ServerId)
		}

		if dstWorld == nil {
			return
		}

		msgSend := &world_message.MUW_ViewFormation{
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
func HandleArenaMatching(con net.Conn, ws *WorldMgr, p proto.Message) {
	if srcWorld := ws.GetWorldByCon(con); srcWorld != nil {
		msg, ok := p.(*world_message.MWU_ArenaMatching)
		if !ok {
			logger.Warning("Cannot assert value to message world_message.MWU_ArenaMatching")
			return
		}

		Instance().GetGameMgr().GetArena().Matching(msg.PlayerId)
	}
}

func HandleArenaAddRecord(con net.Conn, ws *WorldMgr, p proto.Message) {
	if srcWorld := ws.GetWorldByCon(con); srcWorld != nil {
		msg, ok := p.(*world_message.MWU_ArenaAddRecord)
		if !ok {
			logger.Warning("Cannot assert value to message world_message.MWU_ArenaAddRecord")
			return
		}

		Instance().GetGameMgr().GetArena().AddRecord(msg.Record)
	}
}

func HandleArenaBattleResult(con net.Conn, ws *WorldMgr, p proto.Message) {
	if srcWorld := ws.GetWorldByCon(con); srcWorld != nil {
		msg, ok := p.(*world_message.MWU_ArenaBattleResult)
		if !ok {
			logger.Warning("Cannot assert value to message world_message.MWU_ArenaBattleResult")
			return
		}

		Instance().GetGameMgr().GetArena().BattleResult(msg.AttackId, msg.TargetId, msg.AttackWin)
	}
}

func HandleReplacePlayerInfo(con net.Conn, ws *WorldMgr, p proto.Message) {
	if srcWorld := ws.GetWorldByCon(con); srcWorld != nil {
		msg, ok := p.(*world_message.MWU_ReplacePlayerInfo)
		if !ok {
			logger.Warning("Cannot assert value to message world_message.MWU_ReplacePlayerInfo")
			return
		}

		Instance().GetGameMgr().AddPlayerInfo(msg.Info)
	}
}

func HandleReplaceGuildInfo(con net.Conn, ws *WorldMgr, p proto.Message) {
	if srcWorld := ws.GetWorldByCon(con); srcWorld != nil {
		msg, ok := p.(*world_message.MWU_ReplaceGuildInfo)
		if !ok {
			logger.Warning("Cannot assert value to message world_message.MWU_ReplaceGuildInfo")
			return
		}

		Instance().GetGameMgr().AddGuildInfo(msg.Info)
	}
}

func HandleRequestArenaRank(con net.Conn, ws *WorldMgr, p proto.Message) {
	if srcWorld := ws.GetWorldByCon(con); srcWorld != nil {
		msg, ok := p.(*world_message.MWU_RequestArenaRank)
		if !ok {
			logger.Warning("Cannot assert value to message world_message.MWU_RequestArenaRank")
			return
		}

		Instance().GetGameMgr().GetArena().RequestRank(msg.PlayerId, msg.Page)
	}
}

func HandleAddInvite(con net.Conn, ws *WorldMgr, p proto.Message) {
	if srcWorld := ws.GetWorldByCon(con); srcWorld != nil {
		msg, ok := p.(*world_message.MWU_AddInvite)
		if !ok {
			logger.Warning("Cannot assert value to message world_message.MWU_AddInvite")
			return
		}

		Instance().GetGameMgr().GetInvite().AddInvite(msg.NewbieId, msg.InviterId)
	}
}

func HandleCheckInviteResult(con net.Conn, ws *WorldMgr, p proto.Message) {
	if srcWorld := ws.GetWorldByCon(con); srcWorld != nil {
		msg, ok := p.(*world_message.MWU_CheckInviteResult)
		if !ok {
			logger.Warning("Cannot assert value to message world_message.MWU_CheckInviteResult")
			return
		}

		Instance().GetGameMgr().GetInvite().CheckInviteResult(msg.NewbieId, msg.InviterId, msg.ErrorCode)
	}
}

func HandleInviteRecharge(con net.Conn, ws *WorldMgr, p proto.Message) {
	if srcWorld := ws.GetWorldByCon(con); srcWorld != nil {
		msg, ok := p.(*world_message.MWU_InviteRecharge)
		if !ok {
			logger.Warning("Cannot assert value to message world_message.MWU_InviteRecharge")
			return
		}

		Instance().GetGameMgr().GetInvite().InviteRecharge(msg.NewbieId, msg.NewbieName, msg.InviterId, msg.DiamondGift)
	}
}
