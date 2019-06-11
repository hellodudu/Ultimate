package handler

import (
	"context"

	"github.com/hellodudu/Ultimate/iface"
	pbGame "github.com/hellodudu/Ultimate/proto/game"
	log "github.com/sirupsen/logrus"
)

type GameHandler struct {
	gm iface.IGameMgr
}

func NewGameHandler(gm iface.IGameMgr) (*GameHandler, error) {
	return &GameHandler{gm: gm}, nil
}

func (h *GameHandler) GetPlayerInfoByID(ctx context.Context, req *pbGame.GetPlayerInfoByIDRequest, resp *pbGame.GetPlayerInfoByIDReply) error {
	log.Info("GetPlayerInfoByID:", req.Id)

	resp.Info = h.gm.GetPlayerInfoByID(req.Id)
	return nil
}

func (h *GameHandler) GetGuildInfoByID(ctx context.Context, req *pbGame.GetGuildInfoByIDRequest, resp *pbGame.GetGuildInfoByIDReply) error {
	log.Info("GetGuildInfoByID:", req.Id)
	resp.Info = h.gm.GetGuildInfoByID(req.Id)

	return nil
}
