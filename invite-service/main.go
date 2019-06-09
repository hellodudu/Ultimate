package main

import (
	"context"
	"fmt"
	"os"
	"time"

	"github.com/hellodudu/Ultimate/iface"
	"github.com/hellodudu/Ultimate/invite-service/handler"
	"github.com/hellodudu/Ultimate/invite-service/subscriber"
	pbInvite "github.com/hellodudu/Ultimate/proto/invite"
	"github.com/micro/go-micro"
	log "github.com/sirupsen/logrus"
)

func main() {
	// log file
	t := time.Now()
	fileTime := fmt.Sprintf("%d-%d-%d %d-%d-%d", t.Year(), t.Month(), t.Day(), t.Hour(), t.Minute(), t.Second())
	logFn := fmt.Sprintf("log/%s_ultimate_service_invite.log", fileTime)

	file, err := os.OpenFile(logFn, os.O_CREATE|os.O_WRONLY, 0666)
	if err == nil {
		log.SetOutput(file)
	} else {
		log.Info("Failed to log to file, using default stderr")
	}

	// New Service
	service := micro.NewService(
		micro.Name("ultimate.service.invite"),
		micro.Version("latest"),
	)

	// Initialise service
	service.Init()

	// Register Handler
	pbInvite.RegisterExampleHandler(service.Server(), new(handler.Example))

	// Register Struct as Subscriber
	micro.RegisterSubscriber("go.micro.srv.invite-service", service.Server(), new(subscriber.Example))

	// Register Function as Subscriber
	micro.RegisterSubscriber("go.micro.srv.invite-service", service.Server(), subscriber.Handler)

	// Run service
	if err := service.Run(); err != nil {
		log.Fatal(err)
	}
}

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
			invite.log.Info("invite context done!")
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

func (invite *Invite) AddInvite(newbieId int64, inviterId int64) int32 {
	if newbieId == -1 {
		return 3
	}

	if inviterId == -1 {
		return 3
	}

	inviterInfo := invite.gm.GetPlayerInfoByID(inviterId)
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

func (invite *Invite) CheckInviteResult(newbieId int64, inviterId int64, errorCode int32) {
	if newbieId == -1 {
		return
	}

	newbieInfo := invite.gm.GetPlayerInfoByID(newbieId)
	if newbieInfo == nil {
		log.Warn("CheckInviteResult cannot find newbie info:", newbieId)
		return
	}

	if world := invite.wm.GetWorldByID(newbieInfo.ServerId); world != nil {
		msg := &pb.MUW_AddInviteResult{
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

	inviterInfo := invite.gm.GetPlayerInfoByID(inviterId)
	if inviterInfo == nil {
		log.Warn("InviteRecharge cannot find inviter info:", inviterId)
		return
	}

	if world := invite.wm.GetWorldByID(inviterInfo.ServerId); world != nil {
		msg := &pb.MUW_InviteRecharge{
			NewbieId:    newbieId,
			NewbieName:  newbieName,
			InviterId:   inviterId,
			DiamondGift: diamondGift,
		}

		world.SendProtoMessage(msg)
	}
}
