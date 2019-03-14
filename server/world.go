package ultimate

import (
	"context"
	"encoding/binary"
	"fmt"
	"net"
	"sync"
	"time"

	"github.com/golang/protobuf/proto"
	"github.com/hellodudu/Ultimate/global"
	"github.com/hellodudu/Ultimate/logger"
	world_message "github.com/hellodudu/Ultimate/proto"
	"github.com/hellodudu/Ultimate/utils"
)

type World struct {
	Id         uint32      `sql:"id"`   // world id
	Name       string      `sql:"name"` // world name
	Con        net.Conn    // connection
	tHeartBeat *time.Timer // connection heart beat
	tTimeOut   *time.Timer // connection time out
	ctx        context.Context
	cancel     context.CancelFunc
	chw        chan uint32

	mapPlayer map[int64]*world_message.CrossPlayerInfo
	mapGuild  map[int64]*world_message.CrossGuildInfo
	mu        sync.Mutex

	chDBInit chan struct{}
}

func NewWorld(id uint32, name string, con net.Conn, chw chan uint32) *World {
	w := &World{
		Id:         id,
		Name:       name,
		Con:        con,
		tHeartBeat: time.NewTimer(time.Duration(global.WorldHeartBeatSec) * time.Second),
		tTimeOut:   time.NewTimer(time.Duration(global.WorldConTimeOutSec) * time.Second),
		chw:        chw,
		mapPlayer:  make(map[int64]*world_message.CrossPlayerInfo),
		mapGuild:   make(map[int64]*world_message.CrossGuildInfo),
		chDBInit:   make(chan struct{}, 1),
	}

	w.ctx, w.cancel = context.WithCancel(context.Background())
	w.LoadFromDB()
	return w
}

func (w *World) LoadFromDB() {
	query := fmt.Sprintf("select * from world where id = %d", w.Id)
	rows, err := Instance().dbMgr.Query(query)
	if err != nil {
		logger.Warning("world load rom db query<%s> failed:", query, err)
		return
	}

	for rows.Next() {
		var id, time int32
		var name string
		if err := rows.Scan(&id, &name, &time); err != nil {
			logger.Warning("world load query err:", err)
		}
		logger.Info("world load query success:", id, time)
	}

	w.chDBInit <- struct{}{}
}

func (w *World) Stop() {
	w.tHeartBeat.Stop()
	w.tTimeOut.Stop()
	w.cancel()
	w.Con.Close()
}

func (w *World) Run() {
	<-w.chDBInit

	for {
		select {
		// context canceled
		case <-w.ctx.Done():
			logger.Info("world<%d> context done!", w.Id)
			return

		// connecting timeout
		case <-w.tTimeOut.C:
			w.chw <- w.Id

		// Heart Beat
		case <-w.tHeartBeat.C:
			msg := &world_message.MUW_TestConnect{}
			w.SendProtoMessage(msg)
			w.tHeartBeat.Reset(time.Duration(global.WorldHeartBeatSec) * time.Second)
		}
	}
}

func (w *World) ResetTestConnect() {
	w.tHeartBeat.Reset(time.Duration(global.WorldHeartBeatSec) * time.Second)
	w.tTimeOut.Reset(time.Duration(global.WorldConTimeOutSec) * time.Second)
}

func (w *World) SendProtoMessage(p proto.Message) {
	// reply message length = 4bytes size + 8bytes size BaseNetMsg + 2bytes message_name size + message_name + proto_data
	out, err := proto.Marshal(p)
	if err != nil {
		logger.Warning(err)
		return
	}

	typeName := proto.MessageName(p)
	baseMsg := &BaseNetMsg{}
	msgSize := binary.Size(baseMsg) + 2 + len(typeName) + len(out)
	baseMsg.Id = utils.Crc32("MUW_DirectProtoMsg")
	baseMsg.Size = uint32(msgSize)

	var resp []byte = make([]byte, 4+msgSize)
	binary.LittleEndian.PutUint32(resp[:4], uint32(msgSize))
	binary.LittleEndian.PutUint32(resp[4:8], baseMsg.Id)
	binary.LittleEndian.PutUint32(resp[8:12], baseMsg.Size)
	binary.LittleEndian.PutUint16(resp[12:12+2], uint16(len(typeName)))
	copy(resp[14:14+len(typeName)], []byte(typeName))
	copy(resp[14+len(typeName):], out)

	if _, err := w.Con.Write(resp); err != nil {
		logger.Warning("reply message error:", err)
	}
}

func (w *World) SendTransferMessage(data []byte) {
	if _, err := w.Con.Write(data); err != nil {
		logger.Warning("transfer message error:", err)
	}
}

func (w *World) RequestWorldInfo() {
	// request player info
	msgP := &world_message.MUW_RequestPlayerInfo{MinLevel: 17}
	w.SendProtoMessage(msgP)

	// request guild info
	msgG := &world_message.MUW_RequestGuildInfo{}
	w.SendProtoMessage(msgG)
}

func (w *World) PlayUltimateRecord(src_player_id int64, src_server_id uint32, record_id int64, dst_server_id uint32) {
	msg := &world_message.MUW_PlayUltimateRecord{
		SrcPlayerId: src_player_id,
		SrcServerId: src_server_id,
		RecordId:    record_id,
		DstServerId: dst_server_id,
	}
	w.SendProtoMessage(msg)
}

func (w *World) RequestUltimatePlayer(src_player_id int64, src_server_id uint32, dst_player_id int64, dst_server_id uint32) {
	msg := &world_message.MUW_RequestUltimatePlayer{
		SrcPlayerId: src_player_id,
		SrcServerId: src_server_id,
		DstPlayerId: dst_player_id,
		DstServerId: dst_server_id,
	}
	w.SendProtoMessage(msg)
}

func (w *World) QueryWrite(query string) {
	Instance().dbMgr.Exec(query)
}
