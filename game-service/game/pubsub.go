package game

import (
	"context"
	"fmt"
	"reflect"

	"github.com/golang/protobuf/proto"
	"github.com/hellodudu/Ultimate/iface"
	pbPubSub "github.com/hellodudu/Ultimate/proto/pubsub"
	logger "github.com/hellodudu/Ultimate/utils/log"
	"github.com/micro/go-micro"
)

type pubSub struct {
	gm iface.IGameMgr
	wm iface.IWorldMgr

	pubArenaMatching     micro.Publisher
	pubArenaAddRecord    micro.Publisher
	pubArenaBattleResult micro.Publisher
}

func newPubSub(service micro.Service, gm iface.IGameMgr, wm iface.IWorldMgr) *pubSub {
	ps := &pubSub{
		gm: gm,
		wm: wm,
	}

	// create publisher
	ps.pubArenaMatching = micro.NewPublisher("arena.Matching", service.Client())
	ps.pubArenaAddRecord = micro.NewPublisher("arena.AddRecord", service.Client())
	ps.pubArenaBattleResult = micro.NewPublisher("arena.BattleResult", service.Client())

	// register subscriber
	micro.RegisterSubscriber("game.SendWorldMessage", service.Server(), &sendWorldMessageSubHandler{pubsub: ps})
	micro.RegisterSubscriber("game.BroadCast", service.Server(), &broadCastSubHandler{pubsub: ps})

	return ps
}

/////////////////////////////////////
// publish handle
/////////////////////////////////////
func (ps *pubSub) publishArenaMatching(ctx context.Context, m proto.Message) error {
	if err := ps.pubArenaMatching.Publish(ctx, m); err != nil {
		logger.WithFieldsWarn("publish failed", logger.Fields{
			"error":   err,
			"message": proto.MessageName(m),
		})
		return err
	}

	return nil
}

func (ps *pubSub) publishArenaAddRecord(ctx context.Context, m proto.Message) error {
	if err := ps.pubArenaAddRecord.Publish(ctx, m); err != nil {
		logger.WithFieldsWarn("publish failed", logger.Fields{
			"error":   err,
			"message": proto.MessageName(m),
		})
		return err
	}

	return nil
}

func (ps *pubSub) publishArenaBattleResult(ctx context.Context, m proto.Message) error {
	if err := ps.pubArenaBattleResult.Publish(ctx, m); err != nil {
		logger.WithFieldsWarn("publish failed", logger.Fields{
			"error":   err,
			"message": proto.MessageName(m),
		})
		return err
	}

	return nil
}

/////////////////////////////////////
// subscribe handle
/////////////////////////////////////
// broadcast sub handler
type broadCastSubHandler struct {
	pubsub *pubSub
}

func (s *broadCastSubHandler) Process(ctx context.Context, event *pbPubSub.PublishBroadCast) error {
	pType := proto.MessageType(event.MsgName)
	if pType == nil {
		s := fmt.Sprintf("invalid message<%s>, send world message canceled", event.MsgName)
		logger.Error(s)
		return fmt.Errorf(s)
	}

	// prepare proto struct to be unmarshaled in
	msg, ok := reflect.New(pType.Elem()).Interface().(proto.Message)
	if !ok {
		return fmt.Errorf("invalid message<%s>, send world message canceled", event.MsgName)
	}

	// unmarshal
	if err := proto.Unmarshal(event.MsgData, msg); err != nil {
		logger.WithFieldsWarn("Failed to parse proto msg", logger.Fields{
			"msg":   msg,
			"error": err,
		})
		return err
	}

	s.pubsub.wm.BroadCast(msg)
	return nil
}

// sendWorldMessage sub handler
type sendWorldMessageSubHandler struct {
	pubsub *pubSub
}

func (s *sendWorldMessageSubHandler) Process(ctx context.Context, event *pbPubSub.PublishSendWorldMessage) error {
	world := s.pubsub.wm.GetWorldByID(event.Id)
	if world == nil {
		s := fmt.Sprintf("cannot send world message, world<id:%d> isn't exist", event.Id)
		logger.Error(s)
		return fmt.Errorf(s)
	}

	pType := proto.MessageType(event.MsgName)
	if pType == nil {
		s := fmt.Sprintf("invalid message<%s>, send world message canceled", event.MsgName)
		logger.Error(s)
		return fmt.Errorf(s)
	}

	// prepare proto struct to be unmarshaled in
	msg, ok := reflect.New(pType.Elem()).Interface().(proto.Message)
	if !ok {
		return fmt.Errorf("invalid message<%s>, send world message canceled", event.MsgName)
	}

	// unmarshal
	if err := proto.Unmarshal(event.MsgData, msg); err != nil {
		logger.WithFieldsWarn("Failed to parse proto msg", logger.Fields{
			"msg":   msg,
			"error": err,
		})
		return err
	}

	world.SendProtoMessage(msg)

	return nil
}
