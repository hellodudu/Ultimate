package game

import (
	"context"
	"time"

	"github.com/hellodudu/Ultimate/iface"
	"github.com/hellodudu/Ultimate/logger"
	world_message "github.com/hellodudu/Ultimate/proto"
)

type Invite struct {
	gm     iface.IGameMgr
	wm     iface.IWorldMgr
	ctx    context.Context
	cancel context.CancelFunc
}

// NewInvite create new Invite
func NewInvite(ctx context.Context, gm iface.IGameMgr, wm iface.IWorldMgr) (iface.IInvite, error) {
	invite := &Invite{
		gm: gm,
		wm: wm,
	}

	invite.ctx, invite.cancel = context.WithCancel(ctx)

	return invite, nil
}

// Stop stop
func (invite *Invite) Stop() {
	invite.cancel()
}

// Run run
func (invite *Invite) Run() {

	for {
		select {
		// context canceled
		case <-invite.ctx.Done():
			logger.Info("invite context done!")
			return

		default:
			t := time.Now()
			invite.updateTime()
			d := time.Since(t)
			time.Sleep(time.Millisecond - d)
		}
	}
}

func (invite *Invite) updateTime() {

}

func (invite *Invite) AddInvite(newbieId int64, inviterId int64) {
	if newbieId == -1 {
		return
	}

	if inviterId == -1 {
		return
	}

	inviterInfo := Instance().GetGameMgr().GetPlayerInfoByID(inviterId)
	if inviterInfo == nil {
		logger.Warning("AddInvite cannot find inviter info:", inviterId)
		return
	}

	if world := Instance().GetWorldMgr().GetWorldByID(inviterInfo.ServerId); world != nil {
		msg := &world_message.MUW_CheckInvite{
			NewbieId:  newbieId,
			InviterId: inviterId,
		}

		world.SendProtoMessage(msg)
	}
}

func (invite *Invite) CheckInviteResult(newbieId int64, inviterId int64, errorCode int32) {
	if newbieId == -1 {
		return
	}

	newbieInfo := Instance().GetGameMgr().GetPlayerInfoByID(newbieId)
	if newbieInfo == nil {
		logger.Warning("CheckInviteResult cannot find newbie info:", newbieId)
		return
	}

	if world := Instance().GetWorldMgr().GetWorldByID(newbieInfo.ServerId); world != nil {
		msg := &world_message.MUW_AddInviteResult{
			NewbieId:  newbieId,
			InviterId: inviterId,
			ErrorCode: errorCode,
		}

		world.SendProtoMessage(msg)
	}
}

func (invite *Invite) InviteRecharge(newbieId int64, newbieName string, inviterId int64, diamondGift int32) {
	if newbieId == -1 {
		return
	}

	if inviterId == -1 {
		return
	}

	if diamondGift <= 0 {
		return
	}

	inviterInfo := Instance().GetGameMgr().GetPlayerInfoByID(inviterId)
	if inviterInfo == nil {
		logger.Warning("InviteRecharge cannot find inviter info:", inviterId)
		return
	}

	if world := Instance().GetWorldMgr().GetWorldByID(inviterInfo.ServerId); world != nil {
		msg := &world_message.MUW_InviteRecharge{
			NewbieId:    newbieId,
			NewbieName:  newbieName,
			InviterId:   inviterId,
			DiamondGift: diamondGift,
		}

		world.SendProtoMessage(msg)
	}
}
