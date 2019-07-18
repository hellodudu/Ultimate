package handler

import (
	"context"
	"fmt"
	"reflect"

	"github.com/golang/protobuf/proto"
	"github.com/hellodudu/Ultimate/iface"
	"github.com/hellodudu/Ultimate/logger"
	pbGame "github.com/hellodudu/Ultimate/proto/game"
	"github.com/micro/go-micro"
)

type GameHandler struct {
	gm      iface.IGameMgr
	wm      iface.IWorldMgr
	service micro.Service
	pub     micro.Publisher
}

func NewGameHandler(gm iface.IGameMgr, wm iface.IWorldMgr, service micro.Service) (*GameHandler, error) {
	h := &GameHandler{gm: gm, wm: wm, service: service}
	h.pub = micro.NewPublisher("arena", service.Client())
	return h, nil
}

/////////////////////////////////////////////////
// rpc received
/////////////////////////////////////////////////
func (h *GameHandler) GetPlayerInfoByID(ctx context.Context, req *pbGame.GetPlayerInfoByIDRequest, resp *pbGame.GetPlayerInfoByIDReply) error {

	var err error
	if resp.Info, err = h.gm.GetPlayerInfoByID(req.Id); resp.Info == nil {
		return err
	}

	return nil
}

func (h *GameHandler) GetGuildInfoByID(ctx context.Context, req *pbGame.GetGuildInfoByIDRequest, resp *pbGame.GetGuildInfoByIDReply) error {

	var err error
	if resp.Info, err = h.gm.GetGuildInfoByID(req.Id); err != nil {
		return err
	}

	return nil
}

/////////////////////////////////////////////////
// rpc call
/////////////////////////////////////////////////
func (h *GameHandler) SendWorldMessage(ctx context.Context, req *pbGame.SendWorldMessageRequest, resp *pbGame.SendWorldMessageReply) error {

	world := h.wm.GetWorldByID(req.Id)
	if world == nil {
		s := fmt.Sprintf("cannot send world message, world<id:%d> isn't exist", req.Id)
		logger.Error(s)
		return fmt.Errorf(s)
	}

	pType := proto.MessageType(req.MsgName)
	if pType == nil {
		s := fmt.Sprintf("invalid message<%s>, send world message canceled", req.MsgName)
		logger.Error(s)
		return fmt.Errorf(s)
	}

	// prepare proto struct to be unmarshaled in
	msg, ok := reflect.New(pType.Elem()).Interface().(proto.Message)
	if !ok {
		return fmt.Errorf("invalid message<%s>, send world message canceled", req.MsgName)
	}

	// unmarshal
	if err := proto.Unmarshal(req.MsgData, msg); err != nil {
		logger.Warn("Failed to parse proto msg:", msg, err)
		return err
	}

	world.SendProtoMessage(msg)

	return nil
}

func (h *GameHandler) BroadCast(ctx context.Context, req *pbGame.BroadCastRequest, resp *pbGame.BroadCastReply) error {

	pType := proto.MessageType(req.MsgName)
	if pType == nil {
		s := fmt.Sprintf("invalid message<%s>, send world message canceled", req.MsgName)
		logger.Error(s)
		return fmt.Errorf(s)
	}

	// prepare proto struct to be unmarshaled in
	msg, ok := reflect.New(pType.Elem()).Interface().(proto.Message)
	if !ok {
		return fmt.Errorf("invalid message<%s>, send world message canceled", req.MsgName)
	}

	// unmarshal
	if err := proto.Unmarshal(req.MsgData, msg); err != nil {
		logger.Warn("Failed to parse proto msg:", msg, err)
		return err
	}

	h.wm.BroadCast(msg)
	return nil
}
