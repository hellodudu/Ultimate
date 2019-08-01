package world

import (
	"bytes"
	"container/list"
	"context"
	"encoding/binary"
	"fmt"
	"sync"
	"time"

	"github.com/golang/protobuf/proto"
	"github.com/hellodudu/Ultimate/global"
	"github.com/hellodudu/Ultimate/iface"
	"github.com/hellodudu/Ultimate/logger"
	pb "github.com/hellodudu/Ultimate/proto"
	"github.com/hellodudu/Ultimate/utils"
)

type traceMsg struct {
	msgName string
	msgData []byte
}

type world struct {
	ID          uint32         `gorm:"type:int(10);primary_key;column:id;default:0;not null"`
	Name        string         `gorm:"type:varchar(32);column:name;default:'';not null"`
	LastConTime int            `gorm:"type:int(10);column:last_connect_time;default:0;not null"`
	con         iface.ITCPConn // connection
	ds          iface.IDatastore
	tHeartBeat  *time.Timer // connection heart beat
	tTimeOut    *time.Timer // connection time out
	ctx         context.Context
	cancel      context.CancelFunc
	chw         chan uint32

	mapPlayer map[int64]*pb.CrossPlayerInfo
	mapGuild  map[int64]*pb.CrossGuildInfo

	chDBInit chan struct{}

	traceMsgList *list.List
	mu           sync.Mutex
}

func NewWorld(id uint32, name string, con iface.ITCPConn, chw chan uint32, datastore iface.IDatastore) *world {
	w := &world{
		ID:          id,
		Name:        name,
		LastConTime: 0,
		con:         con,
		ds:          datastore,
		tHeartBeat:  time.NewTimer(time.Duration(global.WorldHeartBeatSec) * time.Second),
		tTimeOut:    time.NewTimer(time.Duration(global.WorldConTimeOutSec) * time.Second),
		chw:         chw,
		mapPlayer:   make(map[int64]*pb.CrossPlayerInfo),
		mapGuild:    make(map[int64]*pb.CrossGuildInfo),
		chDBInit:    make(chan struct{}, 1),

		traceMsgList: list.New(),
	}

	w.ctx, w.cancel = context.WithCancel(context.Background())
	w.loadFromDB()
	return w
}

func (world) TableName() string {
	return "world"
}

func (w *world) GetID() uint32 {
	return w.ID
}

func (w *world) GetName() string {
	return w.Name
}

func (w *world) GetCon() iface.ITCPConn {
	return w.con
}

func (w *world) SetLastConTime(t int) {
	w.LastConTime = t
}

func (w *world) loadFromDB() {
	w.ds.DB().First(w)

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
			logger.Info(fmt.Sprintf("world<%d> context done!", w.ID))
			return

		// connecting timeout
		case <-w.tTimeOut.C:
			w.chw <- w.ID

		// Heart Beat
		case <-w.tHeartBeat.C:
			msg := &pb.MUW_TestConnect{}
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
		logger.Warning("send proto msg error:", err)
		return
	}

	// trace message
	w.mu.Lock()
	traceInfo := &traceMsg{msgName: typeName, msgData: make([]byte, len(resp))}
	copy(traceInfo.msgData, resp)
	w.traceMsgList.PushBack(traceInfo)
	if w.traceMsgList.Len() > 5 {
		w.traceMsgList.Remove(w.traceMsgList.Front())
	}
	w.mu.Unlock()
}

func (w *world) SendTransferMessage(data []byte) {
	resp := make([]byte, 4+len(data))
	binary.LittleEndian.PutUint32(resp[:4], uint32(len(data)))
	copy(resp[4:], data)

	if _, err := w.con.Write(resp); err != nil {
		logger.Warning("send transfer msg error:", err)
		return
	}

	// for testing disconnected from world server
	transferMsg := &global.TransferNetMsg{}
	byTransferMsg := make([]byte, binary.Size(transferMsg))

	copy(byTransferMsg, data[:binary.Size(transferMsg)])
	buf := &bytes.Buffer{}
	if _, err := buf.Write(byTransferMsg); err != nil {
		return
	}

	// get top 4 bytes messageid
	if err := binary.Read(buf, binary.LittleEndian, transferMsg); err != nil {
		return
	}

	// trace message
	w.mu.Lock()
	traceInfo := &traceMsg{
		msgName: fmt.Sprintf("transfer_msg id=%d, size=%d, world_id=%d, player_id=%d", transferMsg.ID, transferMsg.Size, transferMsg.WorldID, transferMsg.PlayerID),
		msgData: make([]byte, len(resp)),
	}
	copy(traceInfo.msgData, resp)
	w.traceMsgList.PushBack(traceInfo)
	if w.traceMsgList.Len() > 5 {
		w.traceMsgList.Remove(w.traceMsgList.Front())
	}
	w.mu.Unlock()
}
