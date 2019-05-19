package ultimate

import (
	"context"
	"sync"

	"github.com/hellodudu/Ultimate/logger"
	world_message "github.com/hellodudu/Ultimate/proto"
)

type GameMgr struct {
	arena         *Arena // arena
	invite        *Invite
	mapPlayerInfo sync.Map
	mapGuildInfo  sync.Map
	ctx           context.Context
	cancel        context.CancelFunc
}

func NewGameMgr() (*GameMgr, error) {
	game := &GameMgr{}

	game.ctx, game.cancel = context.WithCancel(context.Background())
	var err error
	game.arena, err = NewArena(game.ctx)
	if err == nil {
		game.arena.LoadFromDB()
	}

	game.invite, err = NewInvite(game.ctx)
	if err == nil {
		game.invite.LoadFromDB()
	}

	return game, err
}

func (g *GameMgr) Run() {
	go g.arena.Run()
	go g.invite.Run()

	for {
		select {
		case <-g.ctx.Done():
			logger.Info("game mgr context done!")
			return
		}
	}
}

func (g *GameMgr) GetArena() *Arena {
	return g.arena
}

func (g *GameMgr) GetInvite() *Invite {
	return g.invite
}

func (g *GameMgr) AddPlayerInfoList(s []*world_message.CrossPlayerInfo) {
	if len(s) == 0 {
		return
	}

	for _, v := range s {
		g.mapPlayerInfo.Store(v.PlayerId, v)
	}

}

func (g *GameMgr) AddPlayerInfo(p *world_message.CrossPlayerInfo) {
	g.mapPlayerInfo.Store(p.PlayerId, p)
}

func (g *GameMgr) AddGuildInfoList(s []*world_message.CrossGuildInfo) {
	if len(s) == 0 {
		return
	}

	for _, v := range s {
		g.mapGuildInfo.Store(v.GuildId, v)
	}

}

func (g *GameMgr) AddGuildInfo(i *world_message.CrossGuildInfo) {
	g.mapGuildInfo.Store(i.GuildId, i)
}

func (g *GameMgr) GetPlayerInfoByID(id int64) *world_message.CrossPlayerInfo {
	v, ok := g.mapPlayerInfo.Load(id)
	if !ok {
		return nil
	}

	value, ok := v.(*world_message.CrossPlayerInfo)
	if !ok {
		return nil
	}

	return value
}

func (g *GameMgr) GetGuildInfoByID(id int64) *world_message.CrossGuildInfo {
	v, ok := g.mapGuildInfo.Load(id)
	if !ok {
		return nil
	}

	value, ok := v.(*world_message.CrossGuildInfo)
	if !ok {
		return nil
	}

	return value
}
