package arena

import (
	"context"

	"github.com/golang/protobuf/proto"
	pbPubSub "github.com/hellodudu/Ultimate/proto/pubsub"
	logger "github.com/hellodudu/Ultimate/utils/log"
	"github.com/micro/go-micro"
)

type pubSub struct {
	pubSendWorldMessage micro.Publisher
	pubBroadCast        micro.Publisher
	arena               *Arena
}

func newPubSub(service micro.Service, arena *Arena) *pubSub {
	ps := &pubSub{
		arena: arena,
	}

	// create publisher
	ps.pubSendWorldMessage = micro.NewPublisher("game.SendWorldMessage", service.Client())
	ps.pubBroadCast = micro.NewPublisher("game.BroadCast", service.Client())

	// register subscriber
	micro.RegisterSubscriber("arena.Matching", service.Server(), &matchingSubHandler{pubsub: ps})
	micro.RegisterSubscriber("arena.AddRecord", service.Server(), &addRecordSubHandler{pubsub: ps})
	micro.RegisterSubscriber("arena.BattleResult", service.Server(), &battleResultSubHandler{pubsub: ps})

	return ps
}

/////////////////////////////////////
// publish handle
/////////////////////////////////////
func (ps *pubSub) publishSendWorldMessage(ctx context.Context, serverID uint32, m proto.Message) error {
	out, err := proto.Marshal(m)
	if err != nil {
		logger.WithFieldsWarn("before publish proto marshal failed", logger.Fields{
			"error":   err,
			"message": proto.MessageName(m),
		})
		return err
	}

	send := &pbPubSub.PublishSendWorldMessage{
		Id:      serverID,
		MsgName: proto.MessageName(m),
		MsgData: out,
	}

	if err := ps.pubSendWorldMessage.Publish(ctx, send); err != nil {
		logger.WithFieldsWarn("publish failed", logger.Fields{
			"error":   err,
			"message": proto.MessageName(send),
		})
		return err
	}

	return nil
}

func (ps *pubSub) publishBroadCast(ctx context.Context, m proto.Message) error {
	out, err := proto.Marshal(m)
	if err != nil {
		logger.WithFieldsWarn("before publish proto marshal failed", logger.Fields{
			"error":   err,
			"message": proto.MessageName(m),
		})
		return err
	}

	send := &pbPubSub.PublishBroadCast{
		MsgName: proto.MessageName(m),
		MsgData: out,
	}

	if err := ps.pubBroadCast.Publish(ctx, send); err != nil {
		logger.WithFieldsWarn("publish failed", logger.Fields{
			"error":   err,
			"message": proto.MessageName(send),
		})
		return err
	}

	return nil
}

/////////////////////////////////////
// subscribe handle
/////////////////////////////////////

// matching handler
type matchingSubHandler struct {
	pubsub *pubSub
}

func (s *matchingSubHandler) Process(ctx context.Context, event *pbPubSub.PublishMatching) error {
	s.pubsub.arena.matching(event.Id)
	return nil
}

// addRecord handler
type addRecordSubHandler struct {
	pubsub *pubSub
}

func (s *addRecordSubHandler) Process(ctx context.Context, event *pbPubSub.PublishAddRecord) error {
	s.pubsub.arena.addRecord(event.Data)
	return nil
}

// battleResult handler
type battleResultSubHandler struct {
	pubsub *pubSub
}

func (s *battleResultSubHandler) Process(ctx context.Context, event *pbPubSub.PublishBattleResult) error {
	s.pubsub.arena.battleResult(event.AttackId, event.TargetId, event.AttackWin)
	return nil
}
