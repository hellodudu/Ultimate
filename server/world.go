package ultimate

import (
	"context"
	"encoding/binary"
	"fmt"
	"log"
	"net"
	"sync"
	"time"

	"github.com/fatih/color"
	"github.com/golang/protobuf/proto"
	"github.com/hellodudu/Ultimate/config"
	"github.com/hellodudu/Ultimate/proto"
)

type World struct {
	Id         uint32      // world id
	Name       string      // world name
	Con        net.Conn    // connection
	tHeartBeat *time.Timer // connection heart beat
	tTimeOut   *time.Timer // connection time out
	ctx        context.Context
	cancel     context.CancelFunc
	chw        chan uint32

	mapPlayer map[int64]*world_message.CrossPlayerInfo
	mapGuild  map[int64]*world_message.CrossGuildInfo
	mu        sync.Mutex

	qWChan  chan string
	cDBInit chan struct{} //
}

func NewWorld(id uint32, name string, con net.Conn, chw chan uint32) *World {
	w := &World{
		Id:         id,
		Name:       name,
		Con:        con,
		tHeartBeat: time.NewTimer(time.Duration(config.WorldHeartBeatSec) * time.Second),
		tTimeOut:   time.NewTimer(time.Duration(config.WorldConTimeOutSec) * time.Second),
		chw:        chw,
		mapPlayer:  make(map[int64]*world_message.CrossPlayerInfo),
		mapGuild:   make(map[int64]*world_message.CrossGuildInfo),
		qWChan:     make(chan string, 100),
		cDBInit:    make(chan struct{}, 1),
	}

	w.ctx, w.cancel = context.WithCancel(context.Background())
	w.LoadFromDB()
	return w
}

func (w *World) LoadFromDB() {
	query := fmt.Sprintf("select * from world where id = %d", w.Id)
	stmt, err := GetUltimateAPI().db.PrepareContext(w.ctx, query)
	if err != nil {
		log.Println(color.YellowString("world <id:", w.Id, "> doing sql prepare failed:", err.Error()))
		return
	}

	rows, err := stmt.QueryContext(w.ctx)
	if err != nil {
		log.Println(color.YellowString("world <id:", w.Id, "> doing sql exec failed:", err.Error()))
		return
	}

	for rows.Next() {
		var id, time int32
		if err := rows.Scan(&id, &time); err != nil {
			log.Println(color.YellowString("world load query err:", err))
		}
		log.Println(color.CyanString("world load query success:", id, time))
	}

	w.cDBInit <- struct{}{}
}

func (w *World) Stop() {
	w.tHeartBeat.Stop()
	w.tTimeOut.Stop()
	w.cancel()
	w.Con.Close()
}

func (w *World) Run() {
	<-w.cDBInit

	for {
		select {
		// context canceled
		case <-w.ctx.Done():
			log.Println(color.RedString("world<%d> context done!", w.Id))
			return

		// connecting timeout
		case <-w.tTimeOut.C:
			w.chw <- w.Id

		// Heart Beat
		case <-w.tHeartBeat.C:
			msg := &world_message.MUW_TestConnect{}
			w.SendMessage(msg)
			w.tHeartBeat.Reset(time.Duration(config.WorldHeartBeatSec) * time.Second)

		// write query
		case q := <-w.qWChan:
			w.Save2DB(q)
		}
	}
}

func (w *World) ResetTestConnect() {
	w.tHeartBeat.Reset(time.Duration(config.WorldHeartBeatSec) * time.Second)
	w.tTimeOut.Reset(time.Duration(config.WorldConTimeOutSec) * time.Second)
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

	if _, err := w.Con.Write(resp); err != nil {
		log.Println(color.YellowString("reply message error:", err.Error()))
	}
}

func (w *World) RequestWorldInfo() {
	// request player info
	msgP := &world_message.MUW_RequestPlayerInfo{MinLevel: 17}
	w.SendMessage(msgP)

	// request guild info
	msgG := &world_message.MUW_RequestGuildInfo{}
	w.SendMessage(msgG)
}

func (w *World) AddPlayerInfo(p *world_message.CrossPlayerInfo) {
	w.mu.Lock()
	w.mapPlayer[p.PlayerId] = p
	w.mu.Unlock()

	log.Println(color.GreenString("add player info:", p))
}

func (w *World) AddPlayerInfoList(s []*world_message.CrossPlayerInfo) {
	if len(s) == 0 {
		return
	}

	w.mu.Lock()

	for _, v := range s {
		w.mapPlayer[v.PlayerId] = v
	}

	w.mu.Unlock()
}

func (w *World) AddGuildInfo(g *world_message.CrossGuildInfo) {
	w.mu.Lock()
	w.mapGuild[g.GuildId] = g
	w.mu.Unlock()

	log.Println(color.GreenString("add guild info:", g))
}

func (w *World) AddGuildInfoList(s []*world_message.CrossGuildInfo) {
	if len(s) == 0 {
		return
	}

	w.mu.Lock()

	for _, v := range s {
		w.mapGuild[v.GuildId] = v
	}

	w.mu.Unlock()
}

func (w *World) QueryWrite(query string) {
	w.qWChan <- query
}

func (w *World) Save2DB(query string) {
	stmt, err := GetUltimateAPI().db.PrepareContext(w.ctx, query)
	if err != nil {
		log.Println(color.YellowString("world <id:", w.Id, "> doing sql prepare failed:", err.Error()))
		return
	}

	res, err := stmt.ExecContext(w.ctx)
	if err != nil {
		log.Println(color.YellowString("world <id:", w.Id, "> doing sql exec failed:", err.Error()))
		return
	}

	_, err = res.RowsAffected()
	if err != nil {
		log.Println(color.WhiteString("world <id:", w.Id, "> doing sql rows affected failed:", err.Error()))
		return
	}

	log.Println(color.CyanString("query successful exec:", query))
}
