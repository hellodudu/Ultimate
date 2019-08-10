package game

import (
	"context"
	"fmt"
	"sync"

	"github.com/hellodudu/Ultimate/iface"
	"github.com/hellodudu/Ultimate/logger"
	pbArena "github.com/hellodudu/Ultimate/proto/arena"
	pbGame "github.com/hellodudu/Ultimate/proto/game"
	pbPubSub "github.com/hellodudu/Ultimate/proto/pubsub"
	"github.com/micro/go-micro"
	client "github.com/micro/go-micro/client"
	"github.com/micro/go-micro/registry/consul"
)

// GameMgr game manager
type GameMgr struct {
	wm            iface.IWorldMgr
	invite        iface.IInvite
	mapPlayerInfo map[int64]*pbGame.CrossPlayerInfo
	mapGuildInfo  map[int64]*pbGame.CrossGuildInfo
	mu            sync.Mutex
	ctx           context.Context
	cancel        context.CancelFunc

	arenaCli pbArena.ArenaServiceClient
	pubsub   *pubSub
}

func NewGameMgr(wm iface.IWorldMgr, service micro.Service) (iface.IGameMgr, error) {
	gm := &GameMgr{
		wm:            wm,
		mapPlayerInfo: make(map[int64]*pbGame.CrossPlayerInfo),
		mapGuildInfo:  make(map[int64]*pbGame.CrossGuildInfo),
	}

	// init invite
	gm.invite = &invite{gm: gm, wm: wm}

	// init arena service client
	gm.arenaCli = pbArena.NewArenaServiceClient(
		"",
		client.NewClient(
			// client.Transport(tcp.NewTransport()),
			client.Registry(consul.NewRegistry()),
		),
	)

	// init context
	gm.ctx, gm.cancel = context.WithCancel(context.Background())

	// Register Handler
	pbGame.RegisterGameServiceHandler(service.Server(), newGameHandler(gm, wm, service))

	// init pub/sub
	gm.pubsub = newPubSub(service, gm, wm)

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
		logger.WithFieldsWarn("GetArenaSeasonData Response", logger.Fields{
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
		logger.WithFieldsWarn("GetArenaChampion Response", logger.Fields{
			"error": err,
		})

		return nil, err
	}

	return rsp.Data, nil
}

func (g *GameMgr) ArenaMatching(id int64) {
	// publish an event
	g.pubsub.publishArenaMatching(g.ctx, &pbPubSub.PublishMatching{Id: id})
}

func (g *GameMgr) ArenaAddRecord(data *pbArena.ArenaRecord) {
	// publish an event
	g.pubsub.publishArenaAddRecord(g.ctx, &pbPubSub.PublishAddRecord{Data: data})
}

func (g *GameMgr) ArenaBattleResult(attackID int64, targetID int64, attackWin bool) {
	// publish an event
	g.pubsub.publishArenaBattleResult(g.ctx, &pbPubSub.PublishBattleResult{AttackId: attackID, TargetId: targetID, AttackWin: attackWin})
}

func (g *GameMgr) ArenaGetRank(id int64, page int32) {
	req := &pbArena.GetRankRequest{PlayerId: id, Page: page}
	_, err := g.arenaCli.GetRank(g.ctx, req)
	if err != nil {
		logger.WithFieldsWarn("ArenaGetRank Response", logger.Fields{
			"error": err,
		})
	}
}

func (g *GameMgr) GetArenaDataNum() int32 {
	req := &pbArena.GetArenaDataNumRequest{}
	rsp, err := g.arenaCli.GetArenaDataNum(g.ctx, req)
	if err != nil {
		logger.WithFieldsWarn("GetArenaDataNum Response", logger.Fields{
			"error": err,
		})
	}

	return rsp.Num
}

func (g *GameMgr) GetArenaRecordNum() int32 {
	req := &pbArena.GetRecordNumRequest{}
	rsp, err := g.arenaCli.GetRecordNum(g.ctx, req)
	if err != nil {
		logger.WithFieldsWarn("GetArenaRecordNum Response", logger.Fields{
			"error": err,
		})
	}

	return rsp.Num
}
