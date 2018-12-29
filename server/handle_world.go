package ultimate

import (
	"log"
	"net"

	"github.com/golang/protobuf/proto"
	"github.com/hellodudu/comment/proto"
)

func HandleWorldLogon(con net.Conn, ws *WorldSession, p proto.Message) {
	world, err := ws.AddWorld(1, "localserver", con)
	if err != nil {
		log.Printf(err.Error())
		return
	}

	rm := &world_message.MUW_WorldLogon{}
	world.SendMessage(rm)
}

func HandleRecvAddressBook(ws *WorldSession, p proto.Message) {

}
