package ultimate

import (
	"context"
	"encoding/binary"
	"fmt"
	"log"
	"net"
	"sync"
	"time"

	"github.com/fatih/color"
	"github.com/golang/protobuf/proto"
	"github.com/hellodudu/Ultimate/config"
	"github.com/hellodudu/Ultimate/proto"
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

	qWChan  chan string
	cDBInit chan struct{} //
}

func NewWorld(id uint32, name string, con net.Conn, chw chan uint32) *World {
	w := &World{
		Id:         id,
		Name:       name,
		Con:        con,
		tHeartBeat: time.NewTimer(time.Duration(config.WorldHeartBeatSec) * time.Second),
		tTimeOut:   time.NewTimer(time.Duration(config.WorldConTimeOutSec) * time.Second),
		chw:        chw,
		mapPlayer:  make(map[int64]*world_message.CrossPlayerInfo),
		mapGuild:   make(map[int64]*world_message.CrossGuildInfo),
		qWChan:     make(chan string, 100),
		cDBInit:    make(chan struct{}, 1),
	}

	w.ctx, w.cancel = context.WithCancel(context.Background())
	w.LoadFromDB()
	return w
}

func (w *World) LoadFromDB() {
	query := fmt.Sprintf("select * from world where id = %d", w.Id)
	stmt, err := Instance().db.PrepareContext(w.ctx, query)
	if err != nil {
		log.Println(color.YellowString("world <id:", w.Id, "> doing sql prepare failed:", err.Error()))
		return
	}

	rows, err := stmt.QueryContext(w.ctx)
	if err != nil {
		log.Println(color.YellowString("world <id:", w.Id, "> doing sql exec failed:", err.Error()))
		return
	}

	for rows.Next() {
		var id, time int32
		var name string
		if err := rows.Scan(&id, &name, &time); err != nil {
			log.Println(color.YellowString("world load query err:", err))
		}
		log.Println(color.CyanString("world load query success:", id, time))
	}

	w.cDBInit <- struct{}{}
}

func (w *World) Stop() {
	w.tHeartBeat.Stop()
	w.tTimeOut.Stop()
	w.cancel()
	w.Con.Close()
}

func (w *World) Run() {
	<-w.cDBInit

	for {
		select {
		// context canceled
		case <-w.ctx.Done():
			log.Println(color.RedString("world<%d> context done!", w.Id))
			return

		// connecting timeout
		case <-w.tTimeOut.C:
			w.chw <- w.Id

		// Heart Beat
		case <-w.tHeartBeat.C:
			msg := &world_message.MUW_TestConnect{}
			w.SendProtoMessage(msg)
			w.tHeartBeat.Reset(time.Duration(config.WorldHeartBeatSec) * time.Second)

		// write query
		case q := <-w.qWChan:
			w.Save2DB(q)
		}
	}
}

func (w *World) ResetTestConnect() {
	w.tHeartBeat.Reset(time.Duration(config.WorldHeartBeatSec) * time.Second)
	w.tTimeOut.Reset(time.Duration(config.WorldConTimeOutSec) * time.Second)
}

func (w *World) SendProtoMessage(p proto.Message) {
	// reply message length = 4bytes size + 8bytes size BaseNetMsg + 2bytes message_name size + message_name + proto_data
	out, err := proto.Marshal(p)
	if err != nil {
		log.Printf(err.Error())
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
		log.Println(color.YellowString("reply message error:", err.Error()))
	}
}

func (w *World) SendTransferMessage(data []byte) {
	if _, err := w.Con.Write(data); err != nil {
		log.Println(color.YellowString("transfer message error:", err.Error()))
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
	w.qWChan <- query
}

func (w *World) Save2DB(query string) {
	stmt, err := Instance().db.PrepareContext(w.ctx, query)
	if err != nil {
		log.Println(color.YellowString("world <id:", w.Id, "> doing sql prepare failed:", err.Error()))
		return
	}

	res, err := stmt.ExecContext(w.ctx)
	if err != nil {
		log.Println(color.YellowString("world <id:", w.Id, "> doing sql exec failed:", err.Error()))
		return
	}

	_, err = res.RowsAffected()
	if err != nil {
		log.Println(color.WhiteString("world <id:", w.Id, "> doing sql rows affected failed:", err.Error()))
		return
	}

	log.Println(color.CyanString("query successful exec:", query))
}
