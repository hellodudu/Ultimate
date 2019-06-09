package invite

import (
	"context"

	pbInvite "github.com/hellodudu/Ultimate/proto/invite"
	log "github.com/sirupsen/logrus"
)

type Invite struct {
	// gm iface.IGameMgr
	// wm iface.IWorldMgr
}

// NewInvite create new Invite
func NewInvite() *Invite {
	i := &Invite{
		// gm: gm,
		// wm: wm,
	}

	return i
}

// Stop stop
func (i *Invite) Stop() {
}

func (i *Invite) AddInvite(newbieId int64, inviterId int64) int32 {
	if newbieId == -1 {
		return 3
	}

	if inviterId == -1 {
		return 3
	}

	inviterInfo := i.gm.GetPlayerInfoByID(inviterId)
	if inviterInfo == nil {
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

func (i *Invite) CheckInviteResult(newbieId int64, inviterId int64, errorCode int32) {
	if newbieId == -1 {
		return
	}

	newbieInfo := i.gm.GetPlayerInfoByID(newbieId)
	if newbieInfo == nil {
		log.Warn("CheckInviteResult cannot find newbie info:", newbieId)
		return
	}

	if world := i.wm.GetWorldByID(newbieInfo.ServerId); world != nil {
		msg := &pb.MUW_AddInviteResult{
			NewbieId:  newbieId,
			InviterId: inviterId,
			ErrorCode: errorCode,
		}

		world.SendProtoMessage(msg)
	}
}

func (i *Invite) InviteRecharge(newbieId int64, newbieName string, inviterId int64, diamondGift int32) {
	if newbieId == -1 {
		return
	}

	if inviterId == -1 {
		return
	}

	if diamondGift <= 0 {
		return
	}

	inviterInfo := i.gm.GetPlayerInfoByID(inviterId)
	if inviterInfo == nil {
		log.Warn("InviteRecharge cannot find inviter info:", inviterId)
		return
	}

	if world := i.wm.GetWorldByID(inviterInfo.ServerId); world != nil {
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
func (i *Invite) Call(ctx context.Context, req *pbInvite.Request, rsp *pbInvite.Response) error {
	log.Info("Received InviteService.Call request")
	rsp.Msg = "Hello " + req.Name
	return nil
}

// Stream is a server side stream handler called via client.Stream or the generated client code
func (i *Invite) Stream(ctx context.Context, req *pbInvite.StreamingRequest, stream pbInvite.InviteService_StreamStream) error {
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
func (i *Invite) PingPong(ctx context.Context, stream pbInvite.InviteService_PingPongStream) error {
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
func (i *Invite) SubHandler(ctx context.Context, msg *pbInvite.Message) error {
	log.Info("Function Received message: ", msg.Say)
	return nil
}
