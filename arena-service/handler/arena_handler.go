package handler

import (
	"context"

	pbGame "github.com/hellodudu/Ultimate/proto/game"
	pbInvite "github.com/hellodudu/Ultimate/proto/invite"
	log "github.com/sirupsen/logrus"
)

type InviteHandler struct {
	// gm iface.IGameMgr
	// wm iface.IWorldMgr
	gameCli pbGame.GameServiceClient
}

// NewInvite create new Invite
func NewInviteHandler() *InviteHandler {
	h := &InviteHandler{
		gameCli: pbGame.NewGameServiceClient("ultimate.service.game", nil),
		// wm: wm,
	}

	return h
}

// Stop stop
func (h *InviteHandler) Stop() {
}

func (h *InviteHandler) AddInvite(newbieId int64, inviterId int64) int32 {
	if newbieId == -1 {
		return 3
	}

	if inviterId == -1 {
		return 3
	}

	req := &pbGame.GetPlayerInfoByIDRequest{Id: inviterId}
	if inviterInfo, err := h.gameCli.GetPlayerInfoByID(context.Background(), req); err != nil {
		log.Warn("AddInvite cannot find inviter info:", inviterId)
		return 3
	}

	if world := invite.wm.GetWorldByID(inviterInfo.ServerId); world != nil {
		msg := &pb.MUW_CheckInvite{
			NewbieId:  newbieId,
			InviterId: inviterId,
		}

		world.SendProtoMessage(msg)
		return 0
	}

	return 3
}

func (h *InviteHandler) CheckInviteResult(newbieId int64, inviterId int64, errorCode int32) {
	if newbieId == -1 {
		return
	}

	newbieInfo := h.gm.GetPlayerInfoByID(newbieId)
	if newbieInfo == nil {
		log.Warn("CheckInviteResult cannot find newbie info:", newbieId)
		return
	}

	if world := h.wm.GetWorldByID(newbieInfo.ServerId); world != nil {
		msg := &pb.MUW_AddInviteResult{
			NewbieId:  newbieId,
			InviterId: inviterId,
			ErrorCode: errorCode,
		}

		world.SendProtoMessage(msg)
	}
}

func (h *InviteHandler) InviteRecharge(newbieId int64, newbieName string, inviterId int64, diamondGift int32) {
	if newbieId == -1 {
		return
	}

	if inviterId == -1 {
		return
	}

	if diamondGift <= 0 {
		return
	}

	inviterInfo := h.gm.GetPlayerInfoByID(inviterId)
	if inviterInfo == nil {
		log.Warn("InviteRecharge cannot find inviter info:", inviterId)
		return
	}

	if world := h.wm.GetWorldByID(inviterInfo.ServerId); world != nil {
		msg := &pb.MUW_InviteRecharge{
			NewbieId:    newbieId,
			NewbieName:  newbieName,
			InviterId:   inviterId,
			DiamondGift: diamondGift,
		}

		world.SendProtoMessage(msg)
	}
}

/////////////////////////////////////
// rpc service
/////////////////////////////////////

// Call is a single request handler called via client.Call or the generated client code
func (h *InviteHandler) Call(ctx context.Context, req *pbInvite.Request, rsp *pbInvite.Response) error {
	log.Info("Received InviteService.Call request")
	rsp.Msg = "Hello " + req.Name
	return nil
}

// Stream is a server side stream handler called via client.Stream or the generated client code
func (h *InviteHandler) Stream(ctx context.Context, req *pbInvite.StreamingRequest, stream pbInvite.InviteService_StreamStream) error {
	log.Info("Received InviteService.Stream request with count: %d", req.Count)

	for i := 0; i < int(req.Count); i++ {
		log.Info("Responding: %d", i)
		if err := stream.Send(&pbInvite.StreamingResponse{
			Count: int64(i),
		}); err != nil {
			return err
		}
	}

	return nil
}

// PingPong is a bidirectional stream handler called via client.Stream or the generated client code
func (h *InviteHandler) PingPong(ctx context.Context, stream pbInvite.InviteService_PingPongStream) error {
	for {
		req, err := stream.Recv()
		if err != nil {
			return err
		}
		log.Info("Got ping %v", req.Stroke)
		if err := stream.Send(&pbInvite.Pong{Stroke: req.Stroke}); err != nil {
			return err
		}
	}
}

/////////////////////////////////////
// subscribe
/////////////////////////////////////
func (h *InviteHandler) SubHandler(ctx context.Context, msg *pbInvite.Message) error {
	log.Info("Function Received message: ", msg.Say)
	return nil
}
