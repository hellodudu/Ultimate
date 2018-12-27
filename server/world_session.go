package ultimate

import (
	"bytes"
	"encoding/binary"
	"errors"
	"log"
	"strings"

	"github.com/golang/protobuf/proto"
	"github.com/hellodudu/comment/proto"
	"github.com/hellodudu/comment/utils"
)

type ProtoHandleFunc func(proto.Message)

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

type World struct {
}

type WorldSession struct {
	worldMap      map[uint32]*World // all connected world
	protoReg      map[uint32]proto.Message
	handleFuncReg map[uint32]ProtoHandleFunc
}

func NewWorldSession() (*WorldSession, error) {
	w := &WorldSession{
		worldMap:      make(map[uint32]*World),
		protoReg:      make(map[uint32]proto.Message),
		handleFuncReg: make(map[uint32]ProtoHandleFunc),
	}

	w.registerAllMessage()
	return w, nil
}

func (ws *WorldSession) registerAllMessage() {
	msgID := utils.Crc32("tutorial.AddressBook")
	ws.regProto(msgID, &tutorial.AddressBook{})
	ws.regProtoHandleFunc(msgID, handleAddressBook)
}

func (ws *WorldSession) getRegProto(msgID uint32) (proto.Message, error) {
	v, ok := ws.protoReg[msgID]
	if ok {
		return v, nil
	}

	return v, errors.New("cannot find proto type registed in world_session!")
}

func (ws *WorldSession) regProto(msgID uint32, m proto.Message) {
	if _, ok := ws.protoReg[msgID]; ok {
		log.Printf("proto msg_id<%d> existed while registing protobuf type!\n", msgID)
		return
	}

	ws.protoReg[msgID] = m
}

func (ws *WorldSession) getProtoHandleFunc(msgID uint32) (ProtoHandleFunc, error) {
	v, ok := ws.handleFuncReg[msgID]
	if ok {
		return v, nil
	}

	return v, errors.New("cannot find proto functor registed in world_session!")
}

func (ws *WorldSession) regProtoHandleFunc(msgID uint32, f ProtoHandleFunc) {
	if _, ok := ws.handleFuncReg[msgID]; ok {
		log.Printf("proto msg_id<%d> existed while registing handle func!\n", msgID)
		return
	}

	ws.handleFuncReg[msgID] = f
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

func (ws *WorldSession) HandleMessage(data []byte) {
	// top 4 bytes are msgSize, next 4 bytes are msgID
	msgID := binary.LittleEndian.Uint32(data[4:8])
	p, err := ws.getRegProto(msgID)
	if err != nil {
		log.Printf("unregisted msgid<%d> received!\n", msgID)
		return
	}

	// begin unmarshal protobuf
	protoUnmarshal(data[8:], p)

	// get proto handler
	f, err := ws.getProtoHandleFunc(msgID)
	if err != nil {
		log.Println(err.Error())
		return
	}

	// callback
	f(p)
}
