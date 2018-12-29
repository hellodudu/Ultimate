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

	reply := &world_message.MUW_WorldLogon{}
	world.SendMessage(reply)
}

func HandleTestConnect(con net.Conn, ws *WorldSession, p proto.Message) {
	if world := ws.GetWorldByCon(con); world != nil {
		reply := &world_message.MUW_TestConnect{}
		world.SendMessage(reply)
	}
}

func HandleHeartBeat(con net.Conn, ws *WorldSession, p proto.Message) {
	if world := ws.GetWorldByCon(con); world != nil {
		reply := &world_message.MUW_HeartBeat{}
		world.SendMessage(reply)
	}
}
