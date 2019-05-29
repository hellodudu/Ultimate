package world

import (
	"context"
	"encoding/binary"
	"fmt"
	"net"
	"time"

	"github.com/golang/protobuf/proto"
	"github.com/hellodudu/Ultimate/global"
	"github.com/hellodudu/Ultimate/iface"
	"github.com/hellodudu/Ultimate/logger"
	world_message "github.com/hellodudu/Ultimate/proto"
	"github.com/hellodudu/Ultimate/utils"
)

type world struct {
	id          uint32   `gorm:"type:int(10);primary_key;column:id;default:0;not null"`
	name        string   `gorm:"type:varchar(32);column:name;default:;not null"`
	lastConTime int      `gorm:"type:int(10);column:last_connect_time;default:0;not null"`
	con         net.Conn // connection
	datastore   iface.IDatastore
	tHeartBeat  *time.Timer // connection heart beat
	tTimeOut    *time.Timer // connection time out
	ctx         context.Context
	cancel      context.CancelFunc
	chw         chan uint32

	mapPlayer map[int64]*world_message.CrossPlayerInfo
	mapGuild  map[int64]*world_message.CrossGuildInfo

	chDBInit chan struct{}
}

func NewWorld(id uint32, name string, con net.Conn, chw chan uint32, datastore iface.IDatastore) iface.IWorld {
	w := &world{
		id:          id,
		name:        name,
		lastConTime: 0,
		con:         con,
		datastore:   datastore,
		tHeartBeat:  time.NewTimer(time.Duration(global.WorldHeartBeatSec) * time.Second),
		tTimeOut:    time.NewTimer(time.Duration(global.WorldConTimeOutSec) * time.Second),
		chw:         chw,
		mapPlayer:   make(map[int64]*world_message.CrossPlayerInfo),
		mapGuild:    make(map[int64]*world_message.CrossGuildInfo),
		chDBInit:    make(chan struct{}, 1),
	}

	w.ctx, w.cancel = context.WithCancel(context.Background())
	w.loadFromDB()
	return w
}

func (w *world) ID() uint32 {
	return w.id
}

func (w *world) Name() string {
	return w.name
}

func (w *world) Con() net.Conn {
	return w.con
}

func (w *world) loadFromDB() {
	query := fmt.Sprintf("select * from world where id = %d", w.id)
	rows, err := w.datastore.Query(query)
	if err != nil {
		logger.Warning(fmt.Sprintf("world load rom db query<%s> failed:", query), err)
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

func (w *world) Stop() {
	w.tHeartBeat.Stop()
	w.tTimeOut.Stop()
	w.cancel()
	w.con.Close()
}

func (w *world) Run() {
	<-w.chDBInit

	for {
		select {
		// context canceled
		case <-w.ctx.Done():
			logger.Info(fmt.Sprintf("world<%d> context done!", w.id))
			return

		// connecting timeout
		case <-w.tTimeOut.C:
			w.chw <- w.id

		// Heart Beat
		case <-w.tHeartBeat.C:
			msg := &world_message.MUW_TestConnect{}
			w.SendProtoMessage(msg)
			w.tHeartBeat.Reset(time.Duration(global.WorldHeartBeatSec) * time.Second)
		}
	}
}

func (w *world) ResetTestConnect() {
	w.tHeartBeat.Reset(time.Duration(global.WorldHeartBeatSec) * time.Second)
	w.tTimeOut.Reset(time.Duration(global.WorldConTimeOutSec) * time.Second)
}

func (w *world) SendProtoMessage(p proto.Message) {
	// reply message length = 4bytes size + 8bytes size BaseNetMsg + 2bytes message_name size + message_name + proto_data
	out, err := proto.Marshal(p)
	if err != nil {
		logger.Warning(err)
		return
	}

	typeName := proto.MessageName(p)
	baseMsg := &global.BaseNetMsg{}
	msgSize := binary.Size(baseMsg) + 2 + len(typeName) + len(out)
	baseMsg.ID = utils.Crc32("MUW_DirectProtoMsg")
	baseMsg.Size = uint32(msgSize)

	var resp []byte = make([]byte, 4+msgSize)
	binary.LittleEndian.PutUint32(resp[:4], uint32(msgSize))
	binary.LittleEndian.PutUint32(resp[4:8], baseMsg.ID)
	binary.LittleEndian.PutUint32(resp[8:12], baseMsg.Size)
	binary.LittleEndian.PutUint16(resp[12:12+2], uint16(len(typeName)))
	copy(resp[14:14+len(typeName)], []byte(typeName))
	copy(resp[14+len(typeName):], out)

	if _, err := w.con.Write(resp); err != nil {
		logger.Warning("reply message error:", err)
	}
}

func (w *world) SendTransferMessage(data []byte) {
	resp := make([]byte, 4+len(data))
	binary.LittleEndian.PutUint32(resp[:4], uint32(len(data)))
	copy(resp[4:], data)
	if _, err := w.con.Write(resp); err != nil {
		logger.Warning("transfer message error:", err)
	}
}
