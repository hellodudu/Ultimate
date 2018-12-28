package ultimate

import (
	"encoding/binary"
	"log"
	"net"

	"github.com/golang/protobuf/proto"
)

func HandleRecvAddressBook(con net.Conn, ws *WorldSession, p proto.Message) {
	world, err := ws.AddWorld(1, "localserver", "127.0.0.1:1234")
	if err != nil {
		log.Printf(err.Error())
	}
	log.Printf("add world result:%v\n", world)

	replyText := []byte("success!")
	var resp []byte = make([]byte, 4+len(replyText))
	binary.LittleEndian.PutUint32(resp[:4], uint32(len(replyText)))
	copy(resp[4:], replyText)
	n, err := con.Write(resp)
	log.Printf("con write bytes<%d>, err<%v>\n", n, err)
}
