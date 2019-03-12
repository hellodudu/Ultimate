package ultimate

import (
	"context"
	"log"
	"sync"

	"github.com/fatih/color"
	world_message "github.com/hellodudu/Ultimate/proto"
)

type GameMgr struct {
	arena         *Arena // arena
	mapPlayerInfo map[int64]*world_message.CrossPlayerInfo
	mapGuildInfo  map[int64]*world_message.CrossGuildInfo
	ctx           context.Context
	cancel        context.CancelFunc
	mu            sync.Mutex
}

func NewGameMgr() (*GameMgr, error) {
	game := &GameMgr{
		mapPlayerInfo: make(map[int64]*world_message.CrossPlayerInfo),
		mapGuildInfo:  make(map[int64]*world_message.CrossGuildInfo),
	}

	game.ctx, game.cancel = context.WithCancel(context.Background())
	var err error
	game.arena, err = NewArena(game.ctx)
	if err == nil {
		game.arena.LoadFromDB()
	}

	return game, err
}

func (g *GameMgr) Run() {
	go g.arena.Run()

	for {
		select {
		case <-g.ctx.Done():
			log.Println(color.CyanString("game mgr context done!"))
			return
		}
	}
}

func (g *GameMgr) GetArena() *Arena {
	return g.arena
}

func (g *GameMgr) AddPlayerInfoList(s []*world_message.CrossPlayerInfo) {
	if len(s) == 0 {
		return
	}

	g.mu.Lock()

	for _, v := range s {
		g.mapPlayerInfo[v.PlayerId] = v
	}

	g.mu.Unlock()
}

func (g *GameMgr) AddPlayerInfo(p *world_message.CrossPlayerInfo) {
	g.mu.Lock()
	g.mapPlayerInfo[p.PlayerId] = p
	g.mu.Unlock()

	log.Println(color.GreenString("add player info:", p))
}

func (g *GameMgr) AddGuildInfoList(s []*world_message.CrossGuildInfo) {
	if len(s) == 0 {
		return
	}

	g.mu.Lock()

	for _, v := range s {
		g.mapGuildInfo[v.GuildId] = v
	}

	g.mu.Unlock()
}

func (g *GameMgr) AddGuildInfo(i *world_message.CrossGuildInfo) {
	g.mu.Lock()
	g.mapGuildInfo[i.GuildId] = i
	g.mu.Unlock()

	log.Println(color.GreenString("add guild info:", i))
}

func (g *GameMgr) GetPlayerInfoByID(id int64) *world_message.CrossPlayerInfo {
	return g.mapPlayerInfo[id]
}

func (g *GameMgr) GetGuildInfoByID(id int64) *world_message.CrossGuildInfo {
	return g.mapGuildInfo[id]
}
