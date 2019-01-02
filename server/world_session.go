package ultimate

import (
	"bytes"
	"encoding/binary"
	"errors"
	"log"
	"net"
	"reflect"
	"sync"
	"time"

	"github.com/golang/protobuf/proto"
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
	mapWorld map[uint32]*World // all connected world
	mapConn  map[net.Conn]*World
	protoReg map[uint32]*regInfo
	wg       sync.WaitGroup
}

func NewWorldSession() (*WorldSession, error) {
	w := &WorldSession{
		mapWorld: make(map[uint32]*World),
		mapConn:  make(map[net.Conn]*World),
		protoReg: make(map[uint32]*regInfo),
	}

	w.registerAllMessage()
	return w, nil
}

func (ws *WorldSession) registerAllMessage() {
	// cb -- callback function
	ws.registerProto(utils.Crc32("world_message.MWU_WorldLogon"), &regInfo{
		cb: HandleWorldLogon,
	})

	ws.registerProto(utils.Crc32("world_message.MWU_TestConnect"), &regInfo{
		cb: HandleTestConnect,
	})

	ws.registerProto(utils.Crc32("world_message.MWU_HeartBeat"), &regInfo{
		cb: HandleHeartBeat,
	})
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

	log.Printf("translate msg to proto:%+v\n", m)
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

func (ws *WorldSession) AddWorld(id uint32, name string, con net.Conn) (*World, error) {
	ws.wg.Wait()
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

	world := &World{Id: id, Name: name, Con: con}
	ws.mapWorld[id] = world
	ws.mapConn[con] = world
	log.Printf("add world<id:%d, %s> success!\n", id, name)

	return world, nil
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
	ws.wg.Wait()
	w, ok := ws.mapConn[con]
	if !ok {
		return
	}

	delete(ws.mapWorld, w.Id)
	delete(ws.mapConn, con)
}

func (ws *WorldSession) Run() {
	for {

		t := time.Now()

		for _, world := range ws.mapWorld {
			log.Println("for range world:", world)
			ws.wg.Add(1)
			world.Run(&ws.wg)
		}

		e := time.Since(t)
		if e < 100*time.Millisecond {
			time.Sleep(100*time.Millisecond - e)
		}
	}
}
