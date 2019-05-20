package game

import (
	"context"
	"sync"

	"github.com/hellodudu/Ultimate/iface"
	"github.com/hellodudu/Ultimate/logger"
	pb "github.com/hellodudu/Ultimate/proto"
)

type GameMgr struct {
	arena         iface.IArena
	invite        iface.IInvite
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

func (g *GameMgr) Arena() iface.IArena {
	return g.arena
}

func (g *GameMgr) Invite() iface.IInvite {
	return g.invite
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

func (g *GameMgr) AddPlayerInfoList(s []*pb.CrossPlayerInfo) {
	if len(s) == 0 {
		return
	}

	for _, v := range s {
		g.mapPlayerInfo.Store(v.PlayerId, v)
	}

}

func (g *GameMgr) AddPlayerInfo(p *pb.CrossPlayerInfo) {
	g.mapPlayerInfo.Store(p.PlayerId, p)
}

func (g *GameMgr) AddGuildInfoList(s []*pb.CrossGuildInfo) {
	if len(s) == 0 {
		return
	}

	for _, v := range s {
		g.mapGuildInfo.Store(v.GuildId, v)
	}

}

func (g *GameMgr) AddGuildInfo(i *pb.CrossGuildInfo) {
	g.mapGuildInfo.Store(i.GuildId, i)
}

func (g *GameMgr) GetPlayerInfoByID(id int64) *pb.CrossPlayerInfo {
	v, ok := g.mapPlayerInfo.Load(id)
	if !ok {
		return nil
	}

	value, ok := v.(*pb.CrossPlayerInfo)
	if !ok {
		return nil
	}

	return value
}

func (g *GameMgr) GetGuildInfoByID(id int64) *pb.CrossGuildInfo {
	v, ok := g.mapGuildInfo.Load(id)
	if !ok {
		return nil
	}

	value, ok := v.(*pb.CrossGuildInfo)
	if !ok {
		return nil
	}

	return value
}
