package ultimate

import (
	"log"

	"github.com/golang/protobuf/proto"
)

func HandleRecvAddressBook(ws *WorldSession, p proto.Message) {
	world, err := ws.AddWorld(1, "localserver", "127.0.0.1:1234")
	if err != nil {
		log.Printf(err.Error())
	}
	log.Printf("add world result:%v\n", world)
}
