package server

import (
	"errors"
	"sync"

	"github.com/go-redis/redis"
	datastore "github.com/hellodudu/Ultimate/db"
	"github.com/hellodudu/Ultimate/game"
	"github.com/hellodudu/Ultimate/global"
	"github.com/hellodudu/Ultimate/iface"
	"github.com/hellodudu/Ultimate/logger"
	"github.com/hellodudu/Ultimate/task"
	"github.com/hellodudu/Ultimate/world"
)

// ultimate define
type ultimate struct {
	td iface.IDispatcher // task dispatcher
	ds iface.IDatastore  // datastore
	wm iface.IWorldMgr   // world manager
	gm iface.IGameMgr    // game manager
	mp iface.IMsgParser  // msg parser

	rds      *redis.Client // redis
	tcpServ  *TcpServer    // tcp server
	rpcServ  *RpcServer    // rpc server
	httpServ *HttpServer   // http server
	wg       sync.WaitGroup
}

func NewUltimate() (iface.IUltimate, error) {
	umt := &ultimate{}

	if ok := logger.Init(global.Debugging); !ok {
		return nil, errors.New("init log file failed")
	}

	umt.InitDatastore()
	umt.InitTask()
	umt.InitWorldMgr()
	umt.InitGame()
	umt.InitMsgParser()
	umt.InitTCPServer()
	umt.InitRPCServer()
	umt.InitHttpServer()

	logger.Print("all init ok!")
	return umt, nil
}

func (umt *ultimate) WorldMgr() iface.IWorldMgr {
	return umt.wm
}

func (umt *ultimate) GameMgr() iface.IGameMgr {
	return umt.gm
}

func (umt *ultimate) Datastore() iface.IDatastore {
	return umt.ds
}

// init task and taskdispatcher
func (umt *ultimate) InitTask() {
	var err error
	if umt.td, err = task.NewDispatcher(); err != nil {
		logger.Fatal(err)
		return
	}

	logger.Print("task init ok!")
}

// init db
func (umt *ultimate) InitDatastore() {
	var err error
	if umt.ds, err = datastore.NewDatastore(); err != nil {
		logger.Fatal(err)
		return
	}
	logger.Print("datastore init ok!")
}

func (umt *ultimate) InitRedis() {
	umt.rds = redis.NewClient(&redis.Options{
		Addr:     global.RedisAddr,
		Password: global.RedisPwd,
		DB:       global.RedisDB,
	})

	if _, err := umt.rds.Ping().Result(); err != nil {
		logger.Fatal(err)
		return
	}

	logger.Print("redis init ok")
}

func (umt *ultimate) InitMsgParser() {
	if umt.mp = NewMsgParser(umt.gm, umt.wm); umt.mp == nil {
		logger.Fatal("cannot new msg_parser")
	}

	logger.Print("msg parser init ok!")
}

// InitTCPServer init
func (umt *ultimate) InitTCPServer() {
	var err error
	if umt.tcpServ, err = NewTcpServer(umt.mp, umt.td); err != nil {
		logger.Fatal(err)
	}

	logger.Print("tcp_server init ok!")
}

// InitRPCServer init
func (umt *ultimate) InitRPCServer() {
	var err error
	if umt.rpcServ, err = NewRpcServer(umt.gm); err != nil {
		logger.Fatal(err)
	}

	logger.Print("rpc_server init ok!")
}

// init http server
func (umt *ultimate) InitHttpServer() {
	if umt.httpServ = NewHttpServer(umt.gm); umt.httpServ == nil {
		logger.Fatal("cannot new http_server")
	}

	logger.Print("http_server init ok!")
}

// init world session
func (umt *ultimate) InitWorldMgr() {
	var err error
	if umt.wm, err = world.NewWorldMgr(umt.ds); err != nil {
		logger.Fatal(err)
	}

	logger.Print("world_session init ok!")
}

func (umt *ultimate) InitGame() {
	var err error
	if umt.gm, err = game.NewGameMgr(umt.wm, umt.ds); err != nil {
		logger.Fatal(err)
	}

	logger.Print("gm init ok!")
}

// run
func (umt *ultimate) Run() {
	go umt.tcpServ.Run()
	go umt.rpcServ.Run()
	go umt.httpServ.Run()
	go umt.wm.Run()
	go umt.gm.Run()
	go umt.ds.Run()

}

func (umt *ultimate) Stop() {
	umt.rpcServ.Stop()
	umt.tcpServ.Stop()
	<-umt.ds.Stop()
	<-umt.wm.Stop()
}
