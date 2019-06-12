package game

import (
	"github.com/hellodudu/Ultimate/iface"
	"github.com/hellodudu/Ultimate/logger"
	pb "github.com/hellodudu/Ultimate/proto/invite"
)

type Invite struct {
	gm iface.IGameMgr
	wm iface.IWorldMgr
}

// NewInvite create new Invite
func NewInvite(gm iface.IGameMgr, wm iface.IWorldMgr) *Invite {
	i := &Invite{
		gm: gm,
		wm: wm,
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
		logger.Warn("AddInvite cannot find inviter info:", inviterId)
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
		logger.Warn("CheckInviteResult cannot find newbie info:", newbieId)
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
		logger.Warn("InviteRecharge cannot find inviter info:", inviterId)
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
