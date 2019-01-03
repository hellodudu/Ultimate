package ultimate

import (
	"context"
	"encoding/binary"
	"log"
	"net"
	"time"

	"github.com/golang/protobuf/proto"
	"github.com/hellodudu/comment/config"
	"github.com/hellodudu/comment/proto"
)

type World struct {
	Id           uint32      // world id
	Name         string      // world name
	Con          net.Conn    // connection
	ConHeartBeat *time.Timer // connection heart beat
	ConTimeOut   *time.Timer // connection time out
	ctx          context.Context
	cancel       context.CancelFunc
	chw          chan uint32
}

func NewWorld(id uint32, name string, con net.Conn, chw chan uint32) *World {
	w := &World{
		Id:           id,
		Name:         name,
		Con:          con,
		ConHeartBeat: time.NewTimer(time.Duration(config.WorldHeartBeatSec) * time.Second),
		ConTimeOut:   time.NewTimer(time.Duration(config.WorldConTimeOutSec) * time.Second),
		chw:          chw,
	}

	w.ctx, w.cancel = context.WithCancel(context.Background())
	return w
}

func (w *World) Stop() {
	w.ConHeartBeat.Stop()
	w.ConTimeOut.Stop()
	w.cancel()
	w.Con.Close()
}

func (w *World) Run() {
	for {
		select {
		// context canceled
		case <-w.ctx.Done():
			return

		// connecting timeout
		case <-w.ConTimeOut.C:
			w.chw <- w.Id

		// Heart Beat
		case <-w.ConHeartBeat.C:
			msg := &world_message.MUW_TestConnect{}
			w.SendMessage(msg)
			w.ConHeartBeat.Reset(time.Duration(config.WorldHeartBeatSec) * time.Second)
		}
	}
}

func (w *World) ResetTestConnect() {
	w.ConHeartBeat.Reset(time.Duration(config.WorldHeartBeatSec) * time.Second)
	w.ConTimeOut.Reset(time.Duration(config.WorldConTimeOutSec) * time.Second)
}

func (w *World) SendMessage(p proto.Message) {
	// reply message length = 4bytes size + 2bytes message_name size + message_name + proto_data
	out, err := proto.Marshal(p)
	if err != nil {
		log.Printf(err.Error())
		return
	}

	typeName := proto.MessageName(p)
	msgSize := 2 + len(typeName) + len(out)

	var resp []byte = make([]byte, 4+msgSize)
	binary.LittleEndian.PutUint32(resp[:4], uint32(msgSize))
	binary.LittleEndian.PutUint16(resp[4:6], uint16(len(typeName)))
	copy(resp[6:6+len(typeName)], []byte(typeName))
	copy(resp[6+len(typeName):], out)

	if _, err := w.Con.Write(resp); err != nil {
		log.Println(err.Error())
	}
}
