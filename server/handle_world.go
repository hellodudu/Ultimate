package ultimate

import (
	"fmt"
	"log"
	"net"
	"time"

	"github.com/fatih/color"
	"github.com/golang/protobuf/proto"
	"github.com/hellodudu/Ultimate/proto"
)

func HandleWorldLogon(con net.Conn, ws *WorldSession, p proto.Message) {
	msg, ok := p.(*world_message.MWU_WorldLogon)
	if !ok {
		log.Println(color.YellowString("Cannot assert value to message world_message.MWU_WorldLogon"))
		return
	}

	world, err := ws.AddWorld(msg.WorldId, msg.WorldName, con)
	if err != nil {
		log.Println(color.YellowString(err.Error()), color.YellowString("<id:%d, name:%s, con:%v>", msg.WorldId, msg.WorldName, con))
		return
	}

	reply := &world_message.MUW_WorldLogon{}
	world.SendMessage(reply)

	// save to db
	query := fmt.Sprintf("replace into world(id, last_connect_time) values(%d, %d)", world.Id, int32(time.Now().Unix()))
	world.QueryWrite(query)
}

func HandleTestConnect(con net.Conn, ws *WorldSession, p proto.Message) {
	if world := ws.GetWorldByCon(con); world != nil {
		world.ResetTestConnect()
	}
}

func HandleHeartBeat(con net.Conn, ws *WorldSession, p proto.Message) {
	if world := ws.GetWorldByCon(con); world != nil {
		reply := &world_message.MUW_HeartBeat{BattleTime: uint32(time.Now().Unix())}
		world.SendMessage(reply)
	}
}

func HandleWorldConnected(con net.Conn, ws *WorldSession, p proto.Message) {
	if world := ws.GetWorldByCon(con); world != nil {
		np := p.(*world_message.MWU_WorldConnected)
		arrWorldID := np.GetWorldId()
		log.Println(color.CyanString("world ref<%v> connected!", arrWorldID))

		world.RequestWorldInfo()
	}
}

func HandleRequestPlayerInfo(con net.Conn, ws *WorldSession, p proto.Message) {
	if world := ws.GetWorldByCon(con); world != nil {
		msg, ok := p.(*world_message.MWU_RequestPlayerInfo)
		if !ok {
			log.Println(color.YellowString("Cannot assert value to message world_message.MWU_RequestPlayerInfo"))
			return
		}

		world.AddPlayerInfoList(msg.Info)
	}
}

func HandleRequestGuildInfo(con net.Conn, ws *WorldSession, p proto.Message) {
	if world := ws.GetWorldByCon(con); world != nil {
		msg, ok := p.(*world_message.MWU_RequestGuildInfo)
		if !ok {
			log.Println(color.YellowString("Cannot assert value to message world_message.MWU_RequestGuildInfo"))
			return
		}

		world.AddGuildInfoList(msg.Info)
	}
}
