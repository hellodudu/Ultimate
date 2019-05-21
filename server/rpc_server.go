package server

import (
	"context"
	"net"
	"sync"

	"github.com/hellodudu/Ultimate/global"
	"github.com/hellodudu/Ultimate/iface"
	"github.com/hellodudu/Ultimate/logger"
	world_message "github.com/hellodudu/Ultimate/proto"
	"google.golang.org/grpc"
)

type RpcServer struct {
	s  map[*grpc.Server]struct{}
	gm iface.IGameMgr
	ln net.Listener
	mu sync.Mutex
}

type rpcResponser struct {
	gm iface.IGameMgr
}

func NewRpcServer(gm iface.IGameMgr) (*RpcServer, error) {
	s := &RpcServer{
		s:  make(map[*grpc.Server]struct{}),
		gm: gm,
	}

	addr, err := global.IniMgr.GetIniValue("config/ultimate.ini", "listen", "RpcListenAddr")
	if err != nil {
		logger.Error("cannot read ini RpcListenAddr!")
		return nil, err
	}

	ln, err := net.Listen("tcp", addr)
	if err != nil {
		logger.Error("rpc server failed to listen: ", err)
		return nil, err
	}

	logger.Print("rpc server listening at ", addr)

	s.ln = ln
	return s, nil
}

// SayHello implements helloworld.GreeterServer
func (s *rpcResponser) SayHello(ctx context.Context, in *world_message.HelloRequest) (*world_message.HelloReply, error) {
	logger.Info("Received: ", in.Name)
	return &world_message.HelloReply{Message: "Reply " + in.Name}, nil
}

func (s *rpcResponser) GetScore(ctx context.Context, in *world_message.GetScoreRequest) (*world_message.GetScoreReply, error) {
	logger.Info("Received: ", in.Id)
	_, err := s.gm.Arena().GetDataByID(in.Id)
	if err != nil {
		logger.Error(err.Error())
		return nil, err
	}

	return &world_message.GetScoreReply{Score: 0}, nil
}

func (server *RpcServer) Run() {
	s := grpc.NewServer()

	server.mu.Lock()
	server.s[s] = struct{}{}
	server.mu.Unlock()

	world_message.RegisterGreeterServer(s, &rpcResponser{gm: server.gm})
	world_message.RegisterInviterServer(s, &rpcResponser{gm: server.gm})
	if err := s.Serve(server.ln); err != nil {
		logger.Error("failed to service rpc Greeter: ", err)
		return
	}
}

func (server *RpcServer) Stop() {
	server.mu.Lock()

	for s := range server.s {
		s.Stop()
		delete(server.s, s)
	}

	server.mu.Unlock()
	server.ln.Close()
}
