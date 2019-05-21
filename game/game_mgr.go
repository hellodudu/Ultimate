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

func NewGameMgr(wm iface.IWorldMgr, ds iface.IDatastore) (iface.IGameMgr, error) {
	gm := &GameMgr{}

	gm.ctx, gm.cancel = context.WithCancel(context.Background())
	var err error
	gm.arena, err = NewArena(gm.ctx, gm, wm, ds)
	if err != nil {
		logger.Fatal(err)
	}

	gm.invite, err = NewInvite(gm.ctx, gm, wm)
	if err != nil {
		logger.Fatal(err)
	}

	return gm, err
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
