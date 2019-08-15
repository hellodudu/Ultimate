package game

import (
	"github.com/hellodudu/Ultimate/iface"
	pb "github.com/hellodudu/Ultimate/proto/game"
	logger "github.com/hellodudu/Ultimate/utils/log"
)

type invite struct {
	gm iface.IGameMgr
	wm iface.IWorldMgr
}

// Stop stop
func (i *invite) Stop() {
}

func (i *invite) AddInvite(newbieID int64, inviterID int64) int32 {
	if newbieID == -1 {
		return 3
	}

	if inviterID == -1 {
		return 3
	}

	inviterInfo, err := i.gm.GetPlayerInfoByID(inviterID)
	if err != nil {
		logger.WithFieldsWarn("AddInvite cannot find inviter info", logger.Fields{
			"error": err,
		})
		return 3
	}

	if world := i.wm.GetWorldByID(inviterInfo.ServerId); world != nil {
		msg := &pb.MUW_CheckInvite{
			NewbieId:  newbieID,
			InviterId: inviterID,
		}

		world.SendProtoMessage(msg)
		return 0
	}

	return 3
}

func (i *invite) CheckInviteResult(newbieID int64, inviterID int64, errorCode int32) {
	if newbieID == -1 {
		return
	}

	newbieInfo, err := i.gm.GetPlayerInfoByID(newbieID)
	if err != nil {
		logger.WithFieldsWarn("CheckInviteResult cannot find newbie info", logger.Fields{
			"error": err,
		})
		return
	}

	if world := i.wm.GetWorldByID(newbieInfo.ServerId); world != nil {
		msg := &pb.MUW_AddInviteResult{
			NewbieId:  newbieID,
			InviterId: inviterID,
			ErrorCode: errorCode,
		}

		world.SendProtoMessage(msg)
	}
}

func (i *invite) InviteRecharge(newbieID int64, newbieName string, inviterID int64, diamondGift int32) {
	if newbieID == -1 {
		return
	}

	if inviterID == -1 {
		return
	}

	if diamondGift <= 0 {
		return
	}

	inviterInfo, err := i.gm.GetPlayerInfoByID(inviterID)
	if err != nil {
		logger.WithFieldsWarn("InviteRecharge cannot find inviter info", logger.Fields{
			"error": err,
		})
		return
	}

	if world := i.wm.GetWorldByID(inviterInfo.ServerId); world != nil {
		msg := &pb.MUW_InviteRecharge{
			NewbieId:    newbieID,
			NewbieName:  newbieName,
			InviterId:   inviterID,
			DiamondGift: diamondGift,
		}

		world.SendProtoMessage(msg)
	}
}
