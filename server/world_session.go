package ultimate

import (
	"bytes"
	"context"
	"encoding/binary"
	"errors"
	"log"
	"net"
	"reflect"
	"sync"

	"github.com/golang/protobuf/proto"
	"github.com/hellodudu/comment/config"
	"github.com/hellodudu/comment/utils"
)

// base net message type define
type BaseNetMsg struct {
	Id   uint32 // message name crc32
	Size uint32 // message size
}

// world logon
type MSG_MWU_WorldLogon struct {
	BaseNetMsg
	WorldID   uint32
	WorldName [32]byte
}

// world session msg register info
type regInfo struct {
	cb func(net.Conn, *WorldSession, proto.Message)
}

type WorldSession struct {
	mapWorld  map[uint32]*World // all connected world
	mapConn   map[net.Conn]*World
	protoReg  map[uint32]*regInfo
	wg        sync.WaitGroup
	ctx       context.Context
	cancel    context.CancelFunc
	cTimeOutW chan uint32
}

func NewWorldSession() (*WorldSession, error) {
	w := &WorldSession{
		mapWorld:  make(map[uint32]*World),
		mapConn:   make(map[net.Conn]*World),
		protoReg:  make(map[uint32]*regInfo),
		cTimeOutW: make(chan uint32, config.WorldConnectMax),
	}

	w.ctx, w.cancel = context.WithCancel(context.Background())

	w.registerAllMessage()
	return w, nil
}

func (ws *WorldSession) Stop() {
	for _, world := range ws.mapWorld {
		world.Stop()
	}

	ws.cancel()
}

func (ws *WorldSession) registerAllMessage() {
	// cb -- callback function
	ws.registerProto(utils.Crc32("world_message.MWU_WorldLogon"), &regInfo{cb: HandleWorldLogon})

	ws.registerProto(utils.Crc32("world_message.MWU_TestConnect"), &regInfo{cb: HandleTestConnect})

	ws.registerProto(utils.Crc32("world_message.MWU_HeartBeat"), &regInfo{cb: HandleHeartBeat})

	ws.registerProto(utils.Crc32("world_message.MWU_WorldConnected"), &regInfo{cb: HandleWorldConnected})
}

func (ws *WorldSession) getRegisterProto(msgID uint32) (*regInfo, error) {
	v, ok := ws.protoReg[msgID]
	if ok {
		return v, nil
	}

	return v, errors.New("cannot find proto type registed in world_session!")
}

func (ws *WorldSession) registerProto(msgID uint32, info *regInfo) {
	if v, ok := ws.protoReg[msgID]; ok {
		log.Printf("register proto msg_id<%d> existed! protobuf type:%v\n", msgID, v)
		return
	}

	ws.protoReg[msgID] = info
}

func binaryUnmarshal(data []byte) {
	msg := &MSG_MWU_WorldLogon{}
	byData := make([]byte, binary.Size(msg))

	// discard top 4 bytes(message size)
	copy(byData, data[4:])

	buf := &bytes.Buffer{}
	if _, err := buf.Write(byData); err != nil {
		log.Fatal(err)
	}

	// get top 4 bytes messageid
	msgID := binary.LittleEndian.Uint32(buf.Bytes()[:4])
	if msgID == utils.Crc32(string("MWU_WorldLogon")) {
		if err := binary.Read(buf, binary.LittleEndian, msg); err != nil {
			log.Fatal(err)
		}
		log.Printf("world<id:%d, name:%s> logon!\n", msg.WorldID, msg.WorldName)
	}

	log.Printf("translate msg:%+v\n", msg)
}

func protoUnmarshal(data []byte, m proto.Message) {
	if err := proto.Unmarshal(data, m); err != nil {
		log.Fatalln("Failed to parse address book:", err)
	}

	log.Printf("translate msg to proto:%T\n", m)
}

func (ws *WorldSession) HandleMessage(con net.Conn, data []byte) {
	// top 4 bytes are msgSize, next 2 bytes are proto name length, the next is proto name, final is proto data.
	protoNameLen := binary.LittleEndian.Uint16(data[4:6])
	protoTypeName := string(data[6 : 6+protoNameLen])
	protoData := data[6+protoNameLen:]
	pType := proto.MessageType(protoTypeName)
	if pType == nil {
		log.Printf("invalid message<%s>, won't deal with it!\n", protoTypeName)
		return
	}

	// unmarshal
	newProto := reflect.New(pType.Elem()).Interface().(proto.Message)
	protoUnmarshal(protoData, newProto)

	msgID := utils.Crc32(protoTypeName)
	r, err := ws.getRegisterProto(msgID)
	if err != nil {
		log.Printf("unregisted msgid<%d> received!\n", msgID)
		return
	}

	// callback
	r.cb(con, ws, newProto)
}

func (ws *WorldSession) addWorld(w *World) {
}

func (ws *WorldSession) AddWorld(id uint32, name string, con net.Conn) (*World, error) {
	ws.wg.Add(1)
	defer ws.wg.Done()
	var invalidID int32 = -1
	if id == uint32(invalidID) {
		return nil, errors.New("add world id invalid!")
	}

	if _, ok := ws.mapWorld[id]; ok {
		return nil, errors.New("add existed world")
	}

	if _, ok := ws.mapConn[con]; ok {
		return nil, errors.New("add existed connection")
	}

	w := NewWorld(id, name, con, ws.cTimeOutW)
	ws.mapWorld[w.Id] = w
	ws.mapConn[w.Con] = w
	log.Printf("add world<id:%d, %s> success!\n", w.Id, w.Name)
	go w.Run()
	return w, nil
}

func (ws *WorldSession) GetWorldByID(id uint32) *World {
	w, ok := ws.mapWorld[id]
	if !ok {
		return nil
	}
	return w
}

func (ws *WorldSession) GetWorldByCon(con net.Conn) *World {
	w, ok := ws.mapConn[con]
	if !ok {
		return nil
	}
	return w
}

func (ws *WorldSession) DisconnectWorld(con net.Conn) {
	ws.wg.Add(1)
	defer ws.wg.Done()
	w, ok := ws.mapConn[con]
	if !ok {
		return
	}

	log.Printf("World<id:%d> disconnected!\n", w.Id)
	w.Stop()

	delete(ws.mapWorld, w.Id)
	delete(ws.mapConn, con)
}

func (ws *WorldSession) KickWorld(id uint32) {
	ws.wg.Add(1)
	defer ws.wg.Done()
	w, ok := ws.mapWorld[id]
	if !ok {
		return
	}

	if _, ok := ws.mapConn[w.Con]; !ok {
		return
	}

	log.Printf("World<id:%d> was kicked by timeout reason!\n", w.Id)
	w.Stop()

	delete(ws.mapConn, w.Con)
	delete(ws.mapWorld, w.Id)
}

func (ws *WorldSession) Run() {
	for {
		select {
		case <-ws.ctx.Done():
			return
		case wid := <-ws.cTimeOutW:
			ws.KickWorld(wid)
		}
	}
}
