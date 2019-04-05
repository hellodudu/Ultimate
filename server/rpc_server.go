import (
	"log"
	"net"

	"github.com/grpc/grpc-go"
	"github.com/hellodudu/Ultimate/global"
	"github.com/hellodudu/Ultimate/logger"
	world_message "github.com/hellodudu/Ultimate/proto"
)

type RpcServer struct {
}

func NewRpcServer() (*RpcServer, error) {
	return &RpcServer{}, nil
}

func (server *RpcServer) Run() {
	addr, err := global.IniMgr.GetIniValue("config/ultimate.ini", "listen", "RpcListenAddr")
	if err != nil {
		logger.Error("cannot read ini RpcListenAddr!")
		return
	}

	lis, err := net.Listen("tcp", addr)
	if err != nil {
		logger.Error("rpc server failed to listen: %v", err)
		return
	}

	s := grpc.NewServer()
	world_message.RegisterGreeterServer(s, &server{})
	if err := s.Serve(lis); err != nil {
		log.Fatalf("failed to serve: %v", err)
	}
}


