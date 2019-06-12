package handler

import (
	"context"

	"github.com/hellodudu/Ultimate/iface"
	"github.com/hellodudu/Ultimate/logger"
	pbGame "github.com/hellodudu/Ultimate/proto/game"
)

type GameHandler struct {
	gm iface.IGameMgr
	wm iface.IWorldMgr
}

func NewGameHandler(gm iface.IGameMgr, wm iface.IWorldMgr) (*GameHandler, error) {
	return &GameHandler{gm: gm, wm: wm}, nil
}

func (h *GameHandler) GetPlayerInfoByID(ctx context.Context, req *pbGame.GetPlayerInfoByIDRequest, resp *pbGame.GetPlayerInfoByIDReply) error {
	logger.Info("GetPlayerInfoByID:", req.Id)

	resp.Info = h.gm.GetPlayerInfoByID(req.Id)
	return nil
}

func (h *GameHandler) GetGuildInfoByID(ctx context.Context, req *pbGame.GetGuildInfoByIDRequest, resp *pbGame.GetGuildInfoByIDReply) error {
	logger.Info("GetGuildInfoByID:", req.Id)
	resp.Info = h.gm.GetGuildInfoByID(req.Id)

	return nil
}

func (h *GameHandler) SendWorldMessage(ctx context.Context, req *pbGame.SendWorldMessageRequest, resp *pbGame.SendWorldMessageReply) error {

	return nil
}

func (h *GameHandler) BroadCast(ctx context.Context, req *pbGame.BroadCastRequest, resp *pbGame.BroadCastReply) error {

	return nil
}
