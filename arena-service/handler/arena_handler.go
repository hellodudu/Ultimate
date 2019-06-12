package handler

import (
	"context"

	pbGame "github.com/hellodudu/Ultimate/proto/game"
	log "github.com/sirupsen/logrus"
)

type ArenaHandler struct {
	// gm iface.IGameMgr
	// wm iface.IWorldMgr
	gameCli pbGame.GameServiceClient
}

// NewArena create new Arena
func NewArenaHandler() *ArenaHandler {
	h := &ArenaHandler{
		gameCli: pbGame.NewGameServiceClient("ultimate.service.game", nil),
		// wm: wm,
	}

	return h
}

// Stop stop
func (h *ArenaHandler) Stop() {
}

/////////////////////////////////////
// rpc service
/////////////////////////////////////

// Call is a single request handler called via client.Call or the generated client code
func (h *ArenaHandler) Call(ctx context.Context, req *pbArena.Request, rsp *pbArena.Response) error {
	log.Info("Received ArenaService.Call request")
	rsp.Msg = "Hello " + req.Name
	return nil
}

// Stream is a server side stream handler called via client.Stream or the generated client code
func (h *ArenaHandler) Stream(ctx context.Context, req *pbArena.StreamingRequest, stream pbArena.ArenaService_StreamStream) error {
	log.Info("Received ArenaService.Stream request with count: %d", req.Count)

	for i := 0; i < int(req.Count); i++ {
		log.Info("Responding: %d", i)
		if err := stream.Send(&pbArena.StreamingResponse{
			Count: int64(i),
		}); err != nil {
			return err
		}
	}

	return nil
}

// PingPong is a bidirectional stream handler called via client.Stream or the generated client code
func (h *ArenaHandler) PingPong(ctx context.Context, stream pbArena.ArenaService_PingPongStream) error {
	for {
		req, err := stream.Recv()
		if err != nil {
			return err
		}
		log.Info("Got ping %v", req.Stroke)
		if err := stream.Send(&pbArena.Pong{Stroke: req.Stroke}); err != nil {
			return err
		}
	}
}

/////////////////////////////////////
// subscribe
/////////////////////////////////////
func (h *ArenaHandler) SubHandler(ctx context.Context, msg *pbArena.Message) error {
	log.Info("Function Received message: ", msg.Say)
	return nil
}
