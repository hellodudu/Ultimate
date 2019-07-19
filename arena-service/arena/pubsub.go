package arena

import (
	"context"

	"github.com/golang/protobuf/proto"
	"github.com/hellodudu/Ultimate/logger"
	pbArena "github.com/hellodudu/Ultimate/proto/arena"
	pbGame "github.com/hellodudu/Ultimate/proto/game"
	"github.com/micro/go-micro"
	"github.com/sirupsen/logrus"
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

	return ps
}

/////////////////////////////////////
// publish handle
/////////////////////////////////////
func (ps *pubSub) publishSendWorldMessage(ctx context.Context, serverID uint32, m proto.Message) error {
	out, err := proto.Marshal(m)
	if err != nil {
		logger.WithFieldsWarn("before publish proto marshal failed", logrus.Fields{
			"error":   err,
			"message": proto.MessageName(m),
		})
		return err
	}

	send := &pbGame.PublishSendWorldMessage{
		Id:      serverID,
		MsgName: proto.MessageName(m),
		MsgData: out,
	}

	if err := ps.pubSendWorldMessage.Publish(ctx, send); err != nil {
		logger.WithFieldsWarn("publish failed", logrus.Fields{
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
		logger.WithFieldsWarn("before publish proto marshal failed", logrus.Fields{
			"error":   err,
			"message": proto.MessageName(m),
		})
		return err
	}

	send := &pbGame.PublishBroadCast{
		MsgName: proto.MessageName(m),
		MsgData: out,
	}

	if err := ps.pubBroadCast.Publish(ctx, send); err != nil {
		logger.WithFieldsWarn("publish failed", logrus.Fields{
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

// process matching sub
func (s *matchingSubHandler) Process(ctx context.Context, event *pbArena.PublishMatching) error {
	s.pubsub.arena.matching(event.Id)
	return nil
}
