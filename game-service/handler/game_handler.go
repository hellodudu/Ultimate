package handler

import (
	"context"

	datastore "github.com/hellodudu/Ultimate/game-service/db"
	pbGame "github.com/hellodudu/Ultimate/proto/game"
	log "github.com/sirupsen/logrus"
)

type GameHandler struct {
	// arena         iface.IArena
	// invite        iface.IInvite
	mapPlayerInfo map[int64]*pbGame.CrossPlayerInfo
	mapGuildInfo  map[int64]*pbGame.CrossGuildInfo
	mu            sync.Lock
	ds            *datastore.Datastore
	// ctx           context.Context
	// cancel        context.CancelFunc
}

func NewGameHandler() (*GameHandler, error) {
	h := &GameHandler{
		mapPlayerInfo: make(map[int64]*pbGame.CrossPlayerInfo),
		mapGuildInfo:  make(map[int64]*pbGame.CrossGuildInfo),
	}

	var err error
	if h.ds, err = datastore.NewDatastore(); err != nil {
		return nil, err
	}

	// gm.ctx, gm.cancel = context.WithCancel(context.Background())
	// gm.arena, err = NewArena(gm.ctx, gm, wm, ds)
	// if err != nil {
	// 	logger.Fatal(err)
	// }

	// gm.invite, err = NewInvite(gm.ctx, gm, wm)
	// if err != nil {
	// 	logger.Fatal(err)
	// }

	return h, nil
}

// func (g *GameMgr) Arena() iface.IArena {
// 	return g.arena
// }

// func (g *GameMgr) Invite() iface.IInvite {
// 	return g.invite
// }

// func (g *GameMgr) Run() {
// 	go g.arena.Run()
// 	go g.invite.Run()

// 	for {
// 		select {
// 		case <-g.ctx.Done():
// 			logger.Info("game mgr context done!")
// 			return
// 		}
// 	}
// }

// func (g *GameMgr) GetArena() iface.IArena {
// 	return g.arena
// }

// func (g *GameMgr) GetInvite() iface.IInvite {
// 	return g.invite
// }

func (h *GameHandler) AddPlayerInfoList(p []*pb.CrossPlayerInfo) {
	if len(p) == 0 {
		return
	}

	h.mu.Lock()
	defer h.mu.Unlock()

	for _, v := range p {
		h.mapPlayerInfo[v.PlayerId] = v
	}
}

func (h *GameHandler) AddPlayerInfo(p *pb.CrossPlayerInfo) {
	h.mu.Lock()
	defer h.mu.Unlock()

	h.mapPlayerInfo[p.PlayerId] = p
}

func (h *GameHandler) AddGuildInfoList(p []*pb.CrossGuildInfo) {
	if len(p) == 0 {
		return
	}

	h.mu.Lock()
	defer h.mu.Unlock()

	for _, v := range p {
		h.mapGuildInfo[v.GuildId] = v
	}

}

func (h *GameHandler) AddGuildInfo(p *pb.CrossGuildInfo) {
	h.mu.Lock()
	defer h.mu.Unlock()

	h.mapGuildInfo[p.GuildId] = p
}

//////////////////////////////////////
// service
//////////////////////////////////////
func (h *GameHandler) GetPlayerInfoByID(ctx context.Context, req *pbGame.GetPlayerInfoByIDRequest, resp *pbGame.GetPlayerInfoByIDReply) error {
	log.Info("GetPlayerInfoByID:", req.Id)

	if v, ok := h.mapPlayerInfo[req.Id]; ok {
		resp.Info = v
	}

	return nil
}

func (h *GameHandler) GetGuildInfoByID(ctx context.Context, req *pbGame.GetGuildInfoByIDRequest, resp *pbGame.GetGuildInfoByIDReply) error {
	log.Info("GetGuildInfoByID:", req.Id)
	if v, ok := h.mapGuildInfo[req.Id]; ok {
		resp.Info = v
	}

	return nil
}
