package game

import (
	"context"

	"github.com/hellodudu/Ultimate/iface"
	"github.com/hellodudu/Ultimate/logger"
	pbArena "github.com/hellodudu/Ultimate/proto/arena"
	pbGame "github.com/hellodudu/Ultimate/proto/game"
	"github.com/sirupsen/logrus"
)

type GameMgr struct {
	invite        iface.IInvite
	mapPlayerInfo map[int64]*pbGame.CrossPlayerInfo
	mapGuildInfo  map[int64]*pbGame.CrossGuildInfo
	mu            sync.Lock
	ctx           context.Context
	cancel        context.CancelFunc

	arenaCli pbArena.ArenaServiceClient
}

func NewGameMgr() (iface.IGameMgr, error) {
	gm := &GameMgr{
		mapPlayerInfo: make(map[int64]*pbGame.CrossPlayerInfo),
		mapGuildInfo:  make(map[int64]*pbGame.CrossGuildInfo),
	}

	gm.invite, err = NewInvite(gm.ctx, gm, wm)
	if err != nil {
		logger.Fatal(err)
	}

	gm.arenaCli = pbArena.NewArenaServiceClient("", nil)

	gm.ctx, gm.cancel = context.WithCancel(context.Background())
	return gm, nil
}

func (g *GameMgr) Invite() iface.IInvite {
	return g.invite
}

func (g *GameMgr) Run() {
	for {
		select {
		case <-g.ctx.Done():
			logger.Info("game mgr context done!")
			return
		}
	}
}

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

func (g *GameMgr) GetArenaSeasonData() (int32, int32, error) {
	req := &pbArena.GetSeasonDataRequest{}
	rsp, err := g.arenaCli.GetSeasonData(g.ctx, req)
	if err != nil {
		logger.WithFieldsWarn("GetArenaSeasonData Response", logrus.Fields{
			"error": err,
		})
		return 0, 0, err
	}

	return rsp.Season, rsp.SeasonEndTime, nil
}

func (g *GameMgr) GetArenaChampion() ([]*pbArena.ArenaChampion, error) {
	req := &pbArena.GetChampionRequest{}
	rsp, err := g.arenaCli.GetChampion(g.ctx, req)
	if err != nil {
		logger.WithFieldsWarn("GetArenaChampion Response", logrus.Fields{
			"error": err,
		})

		return nil, err
	}

	return rsp.Data, nil
}

func (g *GameMgr) Matching(id int64) {
	req := &pbArena.MatchingRequest{}
	rsp, err := g.arenaCli.Matching(g.ctx, req)
	if err != nil {
		logger.WithFieldsWarn("Matching Response", logrus.Fields{
			"error": err,
		})
	}
}
