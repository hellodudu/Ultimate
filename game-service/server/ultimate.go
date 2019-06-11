package server

import (
	"fmt"
	"os"
	"os/signal"
	"sync"
	"syscall"

	"github.com/go-redis/redis"
	datastore "github.com/hellodudu/Ultimate/game-service/db"
	"github.com/hellodudu/Ultimate/game-service/handler"
	"github.com/hellodudu/Ultimate/global"
	"github.com/hellodudu/Ultimate/iface"
	"github.com/hellodudu/Ultimate/logger"
	"github.com/hellodudu/Ultimate/task"
	"github.com/hellodudu/Ultimate/world"
	"github.com/liangdas/mqant/log"
	"github.com/micro/go-micro"
)

// ultimate define
type ultimate struct {
	td iface.IDispatcher // task dispatcher
	ds iface.IDatastore  // datastore
	wm iface.IWorldMgr   // world manager
	mp iface.IMsgParser  // msg parser

	gameSrv     micro.Service
	gameHandler *handler.GameHandler

	rds      *redis.Client // redis
	tcpServ  *TCPServer    // tcp server
	rpcServ  *RpcServer    // rpc server
	httpServ *HttpServer   // http server
	wg       sync.WaitGroup
}

func NewUltimate() (iface.IUltimate, error) {
	umt := &ultimate{}

	if err := umt.InitDatastore(); err != nil {
		return nil, err
	}

	if err := umt.InitTask(); err != nil {
		return nil, err
	}

	umt.InitWorldMgr()

	if err := umt.InitGame(); err != nil {
		return nil, err
	}

	umt.InitMsgParser()
	umt.InitTCPServer()
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
func (umt *ultimate) InitTask() error {
	var err error
	if umt.td, err = task.NewDispatcher(); err != nil {
		return err
	}

	log.Info("task init ok!")

	return nil
}

// init db
func (umt *ultimate) InitDatastore() error {
	var err error
	if umt.ds, err = datastore.NewDatastore(); err != nil {
		return err
	}

	log.Info("datastore init ok!")
	return nil
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

func (umt *ultimate) InitGame() error {

	var err error
	if umt.gameHandler, err = handler.NewGameHandler(); err != nil {
		return err
	}

	// New Service
	umt.gameSrv = micro.NewService(
		micro.Name("ultimate.service.game"),
		micro.Version("latest"),
	)

	// Initialise service
	umt.gameSrv.Init()

	// Register Handler
	pbGame.RegisterGameServiceHandler(umt.gameSrv.Server(), umt.gameHandler)

	log.Info("game init ok!")
	return nil
}

// run
func (umt *ultimate) Run() {
	go umt.tcpServ.Run()
	go umt.rpcServ.Run()
	go umt.httpServ.Run()
	go umt.wm.Run()
	go umt.gm.Run()
	go umt.ds.Run()

	// rpc service
	go func() {
		if err := umt.gameSrv.Run(); err != nil {
			log.Fatal(err)
		}
	}()

	// server exit
	c := make(chan os.Signal, 1)
	signal.Notify(c, syscall.SIGHUP, syscall.SIGQUIT, syscall.SIGTERM, syscall.SIGINT)
	for {
		sig := <-c
		log.Info(fmt.Sprintf("ultimate server closing down (signal: %v)", sig))

		switch sig {
		case syscall.SIGQUIT, syscall.SIGTERM, syscall.SIGSTOP, syscall.SIGINT:
			umt.Stop()
			log.Info("server exit safely")
			return
		case syscall.SIGHUP:
		default:
			return
		}
	}
}

func (umt *ultimate) Stop() {
	umt.rpcServ.Stop()
	umt.tcpServ.Stop()
	<-umt.ds.Stop()
	<-umt.wm.Stop()
}
