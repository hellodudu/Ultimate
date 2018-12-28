package ultimate

import (
	"bytes"
	"encoding/binary"
	"errors"
	"log"
	"net"
	"reflect"
	"strings"

	"github.com/golang/protobuf/proto"
	"github.com/hellodudu/comment/proto"
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
	protoReg map[uint32]*regInfo
}

func NewWorldSession() (*WorldSession, error) {
	w := &WorldSession{
		mapWorld: make(map[uint32]*World),
		protoReg: make(map[uint32]*regInfo),
	}

	w.registerAllMessage()
	return w, nil
}

func (ws *WorldSession) registerAllMessage() {
	// cb -- callback function
	ws.regProto(utils.Crc32("tutorial.AddressBook"), &regInfo{
		cb: HandleRecvAddressBook,
	})
}

func (ws *WorldSession) getRegProto(msgID uint32) (*regInfo, error) {
	v, ok := ws.protoReg[msgID]
	if ok {
		return v, nil
	}

	return v, errors.New("cannot find proto type registed in world_session!")
}

func (ws *WorldSession) regProto(msgID uint32, info *regInfo) {
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

func protoMarshal(book *tutorial.AddressBook) []byte {
	p := &tutorial.Person{}
	p.Name = strings.TrimSpace("hellodudu")
	p.Id = 11000001
	p.Email = strings.TrimSpace("hellodudu86@gmail.com")
	pn := &tutorial.Person_PhoneNumber{
		Number: strings.TrimSpace("13401039297"),
		Type:   tutorial.Person_MOBILE,
	}
	p.Phones = append(p.Phones, pn)
	book.People = append(book.People, p)
	out, err := proto.Marshal(book)
	log.Println("marshal proto byte:", out, ", size:", len(out))
	if err != nil {
		log.Fatal(err)
	}
	return out
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
	r, err := ws.getRegProto(msgID)
	if err != nil {
		log.Printf("unregisted msgid<%d> received!\n", msgID)
		return
	}

	// callback
	r.cb(con, ws, newProto)
}

func (ws *WorldSession) AddWorld(id uint32, name string, addr string) (*World, error) {
	var invalidID int32 = -1
	if id == uint32(invalidID) {
		return nil, errors.New("add world id invalid!")
	}

	if _, ok := ws.mapWorld[id]; ok {
		return nil, errors.New("add existed world")
	}

	world := &World{Id: id, Name: name, Addr: addr}
	ws.mapWorld[id] = world
	log.Printf("add world<id:%d, %s> success!\n", id, name)

	return world, nil
}
