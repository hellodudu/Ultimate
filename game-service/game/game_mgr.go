package game

import (
	"context"
	"fmt"
	"sync"

	"github.com/hellodudu/Ultimate/iface"
	pbArena "github.com/hellodudu/Ultimate/proto/arena"
	pbGame "github.com/hellodudu/Ultimate/proto/game"
	pbPubSub "github.com/hellodudu/Ultimate/proto/pubsub"
	"github.com/micro/go-micro/v2"
	log "github.com/rs/zerolog/log"
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

	arenaSrv pbArena.ArenaService
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
	gm.arenaSrv = pbArena.NewArenaService(
		"ultimate-service-arena",
		service.Client(),
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
			log.Info().Msg("game mgr context done!")
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
	g.mu.Lock()
	defer g.mu.Unlock()

	if v, ok := g.mapPlayerInfo[id]; ok {
		return v, nil
	}

	return nil, fmt.Errorf("cannot find player by id:%d", id)
}

func (g *GameMgr) GetGuildInfoByID(id int64) (*pbGame.CrossGuildInfo, error) {
	g.mu.Lock()
	defer g.mu.Unlock()

	if v, ok := g.mapGuildInfo[id]; ok {
		return v, nil
	}

	return nil, fmt.Errorf("cannot find guild by id:%d", id)
}

func (g *GameMgr) GetArenaSeasonData() (int32, int32, error) {
	req := &pbArena.GetSeasonDataRequest{}
	rsp, err := g.arenaSrv.GetSeasonData(g.ctx, req)
	if err != nil {
		log.Warn().
			Err(err).
			Msg("GetArenaSeasonData Response")
		return 0, 0, err
	}

	return rsp.Season, rsp.SeasonEndTime, nil
}

func (g *GameMgr) GetArenaChampion() ([]*pbArena.ArenaChampion, error) {
	req := &pbArena.GetChampionRequest{}
	rsp, err := g.arenaSrv.GetChampion(g.ctx, req)
	if err != nil {
		log.Warn().
			Err(err).
			Msg("GetArenaSeasonData Response")

		return nil, err
	}

	return rsp.Data, nil
}

func (g *GameMgr) GetArenaMatchingList() ([]int64, error) {
	req := &pbArena.GetMatchingListRequest{}
	rsp, err := g.arenaSrv.GetMatchingList(g.ctx, req)
	if err != nil {
		log.Warn().
			Err(err).
			Msg("GetMatchingList Response")
		return rsp.Ids, nil
	}

	return nil, err
}

func (g *GameMgr) GetArenaRecordReqList() ([]*pbArena.RecordReqList, error) {
	req := &pbArena.GetRecordReqListRequest{}
	rsp, err := g.arenaSrv.GetRecordReqList(g.ctx, req)
	if err != nil {
		log.Warn().
			Err(err).
			Msg("GetRecordReqList Response")
		return rsp.ReqList, nil
	}

	return nil, err
}

func (g *GameMgr) GetArenaRecord(id int64) (*pbArena.ArenaRecord, error) {
	rpcReq := &pbArena.GetRecordByIDRequest{Id: id}
	rsp, err := g.arenaSrv.GetRecordByID(g.ctx, rpcReq)
	if err != nil {
		log.Warn().
			Err(err).
			Msg("GetRecordByID Response")
		return nil, err
	}

	return rsp.Record, nil
}

func (g *GameMgr) GetArenaRankList(page int) ([]byte, error) {
	rpcReq := &pbArena.GetRankListByPageRequest{Page: int32(page)}
	rsp, err := g.arenaSrv.GetRankListByPage(g.ctx, rpcReq)
	if err != nil {
		log.Warn().
			Err(err).
			Msg("GetRankListByPage Response")
		return nil, err
	}

	return rsp.Data, nil
}

func (g *GameMgr) ArenaAPIRequestRank(id int64, page int) ([]byte, error) {
	rpcReq := &pbArena.GetRankListByPageRequest{Page: int32(page)}
	rsp, err := g.arenaSrv.GetRankListByPage(g.ctx, rpcReq)
	if err != nil {
		log.Warn().
			Err(err).
			Msg("GetRankListByPage Response")
		return nil, err
	}

	return rsp.Data, nil
}

func (g *GameMgr) ArenaSaveChampion() error {
	req := &pbArena.SaveChampionRequest{}
	_, err := g.arenaSrv.SaveChampion(g.ctx, req)
	if err != nil {
		log.Warn().
			Err(err).
			Msg("SaveChampion Response")
		return err
	}

	return nil
}

func (g *GameMgr) ArenaWeekEnd() error {
	req := &pbArena.WeekEndRequest{}
	_, err := g.arenaSrv.WeekEnd(g.ctx, req)
	if err != nil {
		log.Warn().
			Err(err).
			Msg("WeekEnd Response")
		return err
	}

	return nil
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
	_, err := g.arenaSrv.GetRank(g.ctx, req)
	if err != nil {
		log.Warn().
			Err(err).
			Msg("ArenaGetRank Response")
	}
}

func (g *GameMgr) GetArenaDataNum() int32 {
	req := &pbArena.GetArenaDataNumRequest{}
	rsp, err := g.arenaSrv.GetArenaDataNum(g.ctx, req)
	if err != nil {
		log.Warn().
			Err(err).
			Msg("GetArenaDataNum Response")
	}

	return rsp.Num
}

func (g *GameMgr) GetArenaRecordNum() int32 {
	req := &pbArena.GetRecordNumRequest{}
	rsp, err := g.arenaSrv.GetRecordNum(g.ctx, req)
	if err != nil {
		log.Warn().
			Err(err).
			Msg("GetArenaRecordNum Response")
	}

	return rsp.Num
}
