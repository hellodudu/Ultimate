package game

import (
	"context"

	datastore "github.com/hellodudu/Ultimate/game-service/db"
	"github.com/hellodudu/Ultimate/iface"
	pbGame "github.com/hellodudu/Ultimate/proto/game"
	log "github.com/sirupsen/logrus"
)

type GameMgr struct {
	// arena         iface.IArena
	invite        iface.IInvite
	mapPlayerInfo map[int64]*pbGame.CrossPlayerInfo
	mapGuildInfo  map[int64]*pbGame.CrossGuildInfo
	mu            sync.Lock
	ds            *datastore.Datastore
	ctx           context.Context
	cancel        context.CancelFunc
}

func NewGameMgr() (iface.IGameMgr, error) {
	gm := &GameMgr{
		mapPlayerInfo: make(map[int64]*pbGame.CrossPlayerInfo),
		mapGuildInfo:  make(map[int64]*pbGame.CrossGuildInfo),
	}

	var err error
	if gm.ds, err = datastore.NewDatastore(); err != nil {
		return nil, err
	}

	gm.ctx, gm.cancel = context.WithCancel(context.Background())
	// gm.arena, err = NewArena(gm.ctx, gm, wm, ds)
	// if err != nil {
	// 	logger.Fatal(err)
	// }

	gm.invite, err = NewInvite(gm.ctx, gm, wm)
	if err != nil {
		logger.Fatal(err)
	}

	return gm, nil
}

// func (g *GameMgr) Arena() iface.IArena {
// 	return g.arena
// }

func (g *GameMgr) Invite() iface.IInvite {
	return g.invite
}

func (g *GameMgr) Run() {
	// go g.arena.Run()

	for {
		select {
		case <-g.ctx.Done():
			log.Info("game mgr context done!")
			return
		}
	}
}

// func (g *GameMgr) GetArena() iface.IArena {
// 	return g.arena
// }

func (g *GameMgr) GetInvite() iface.IInvite {
	return g.invite
}

func (g *GameMgr) AddPlayerInfoList(p []*pbGame.CrossPlayerInfo) {
	if len(p) == 0 {
		return
	}

	g.mu.Lock()
	defer g.mu.Unlock()

	for _, v := range p {
		g.mapPlayerInfo[v.PlayerId] = v
	}
}

func (g *GameMgr) AddPlayerInfo(p *pbGame.CrossPlayerInfo) {
	g.mu.Lock()
	defer g.mu.Unlock()

	g.mapPlayerInfo[p.PlayerId] = p
}

func (g *GameMgr) AddGuildInfoList(p []*pbGame.CrossGuildInfo) {
	if len(p) == 0 {
		return
	}

	g.mu.Lock()
	defer g.mu.Unlock()

	for _, v := range p {
		g.mapGuildInfo[v.GuildId] = v
	}

}

func (g *GameMgr) AddGuildInfo(p *pbGame.CrossGuildInfo) {
	g.mu.Lock()
	defer g.mu.Unlock()

	g.mapGuildInfo[p.GuildId] = p
}

func (g *GameMgr) GetPlayerInfoByID(id int64) *pbGame.CrossPlayerInfo {
	if v, ok := g.mapPlayerInfo[id]; ok {
		return v
	}

	return nil
}

func (g *GameMgr) GetGuildInfoByID(id int64) *pbGame.CrossGuildInfo {
	if v, ok := g.mapGuildInfo[id]; ok {
		return v
	}

	return nil
}
