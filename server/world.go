package ultimate

import (
	"encoding/binary"
	"log"
	"net"

	"github.com/golang/protobuf/proto"
)

type World struct {
	Id   uint32   // world id
	Name string   // world name
	Con  net.Conn // connection
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
	n, err := w.Con.Write(resp)
}
