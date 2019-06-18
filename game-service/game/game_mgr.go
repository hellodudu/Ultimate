package game

import (
	"context"
	"fmt"
	"sync"

	"github.com/hellodudu/Ultimate/iface"
	"github.com/hellodudu/Ultimate/logger"
	pbArena "github.com/hellodudu/Ultimate/proto/arena"
	pbGame "github.com/hellodudu/Ultimate/proto/game"
	"github.com/sirupsen/logrus"
)

type GameMgr struct {
	wm            iface.IWorldMgr
	invite        iface.IInvite
	mapPlayerInfo map[int64]*pbGame.CrossPlayerInfo
	mapGuildInfo  map[int64]*pbGame.CrossGuildInfo
	mu            sync.Mutex
	ctx           context.Context
	cancel        context.CancelFunc

	arenaCli pbArena.ArenaServiceClient
}

func NewGameMgr(wm iface.IWorldMgr) (iface.IGameMgr, error) {
	gm := &GameMgr{
		wm:            wm,
		mapPlayerInfo: make(map[int64]*pbGame.CrossPlayerInfo),
		mapGuildInfo:  make(map[int64]*pbGame.CrossGuildInfo),
	}

	gm.invite = NewInvite(gm, wm)
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

func (g *GameMgr) GetPlayerInfoByID(id int64) (*pbGame.CrossPlayerInfo, error) {
	if v, ok := g.mapPlayerInfo[id]; ok {
		return v, nil
	}

	return nil, fmt.Errorf("cannot find player by id:%d", id)
}

func (g *GameMgr) GetGuildInfoByID(id int64) (*pbGame.CrossGuildInfo, error) {
	if v, ok := g.mapGuildInfo[id]; ok {
		return v, nil
	}

	return nil, fmt.Errorf("cannot find guild by id:%d", id)
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

func (g *GameMgr) ArenaMatching(id int64) {
	req := &pbArena.MatchingRequest{Id: id}
	_, err := g.arenaCli.Matching(g.ctx, req)
	if err != nil {
		logger.WithFieldsWarn("ArenaMatching Response", logrus.Fields{
			"error": err,
		})
	}
}

func (g *GameMgr) ArenaAddRecord(data *pbArena.ArenaRecord) {
	req := &pbArena.AddRecordRequest{Data: data}
	_, err := g.arenaCli.AddRecord(g.ctx, req)
	if err != nil {
		logger.WithFieldsWarn("ArenaAddRecord Response", logrus.Fields{
			"error": err,
		})
	}
}

func (g *GameMgr) ArenaBattleResult(attackID int64, targetID int64, attackWin bool) {
	req := &pbArena.BattleResultRequest{AttackId: attackID, TargetId: targetID, AttackWin: attackWin}
	_, err := g.arenaCli.BattleResult(g.ctx, req)
	if err != nil {
		logger.WithFieldsWarn("ArenaBattleResult Response", logrus.Fields{
			"error": err,
		})
	}
}

func (g *GameMgr) ArenaGetRank(id int64, page int32) {
	req := &pbArena.GetRankRequest{PlayerId: id, Page: page}
	_, err := g.arenaCli.GetRank(g.ctx, req)
	if err != nil {
		logger.WithFieldsWarn("ArenaGetRank Response", logrus.Fields{
			"error": err,
		})
	}
}

func (g *GameMgr) GetArenaDataNum() int32 {
	req := &pbArena.GetArenaDataNumRequest{}
	rsp, err := g.arenaCli.GetArenaDataNum(g.ctx, req)
	if err != nil {
		logger.WithFieldsWarn("GetArenaDataNum Response", logrus.Fields{
			"error": err,
		})
	}

	return rsp.Num
}

func (g *GameMgr) GetArenaRecordNum() int32 {
	req := &pbArena.GetRecordNumRequest{}
	rsp, err := g.arenaCli.GetRecordNum(g.ctx, req)
	if err != nil {
		logger.WithFieldsWarn("GetArenaRecordNum Response", logrus.Fields{
			"error": err,
		})
	}

	return rsp.Num
}
