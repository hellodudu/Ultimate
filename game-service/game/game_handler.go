package game

import (
	"context"

	"github.com/hellodudu/Ultimate/iface"
	pbGame "github.com/hellodudu/Ultimate/proto/game"
	"github.com/micro/go-micro"
)

type gameHandler struct {
	gm      iface.IGameMgr
	wm      iface.IWorldMgr
	service micro.Service
	pub     micro.Publisher
}

func newGameHandler(gm iface.IGameMgr, wm iface.IWorldMgr, service micro.Service) *gameHandler {
	h := &gameHandler{gm: gm, wm: wm, service: service}
	h.pub = micro.NewPublisher("arena", service.Client())
	return h
}

/////////////////////////////////////////////////
// rpc received
/////////////////////////////////////////////////
func (h *gameHandler) GetPlayerInfoByID(ctx context.Context, req *pbGame.GetPlayerInfoByIDRequest, resp *pbGame.GetPlayerInfoByIDReply) error {

	var err error
	if resp.Info, err = h.gm.GetPlayerInfoByID(req.Id); resp.Info == nil {
		return err
	}

	return nil
}

func (h *gameHandler) GetGuildInfoByID(ctx context.Context, req *pbGame.GetGuildInfoByIDRequest, resp *pbGame.GetGuildInfoByIDReply) error {

	var err error
	if resp.Info, err = h.gm.GetGuildInfoByID(req.Id); err != nil {
		return err
	}

	return nil
}
