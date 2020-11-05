package server

import (
	"os"
	"os/signal"
	"sync"
	"syscall"

	"github.com/go-redis/redis"
	datastore "github.com/hellodudu/Ultimate/game-service/db"
	"github.com/hellodudu/Ultimate/game-service/game"
	"github.com/hellodudu/Ultimate/game-service/world"
	"github.com/hellodudu/Ultimate/iface"
	"github.com/hellodudu/Ultimate/utils"
	"github.com/hellodudu/Ultimate/utils/global"
	"github.com/hellodudu/Ultimate/utils/task"
	"github.com/micro/go-micro/v2"
	"github.com/micro/go-plugins/wrapper/monitoring/prometheus/v2"
	log "github.com/rs/zerolog/log"
)

// ultimate define
type ultimate struct {
	td iface.IDispatcher // task dispatcher
	ds iface.IDatastore  // datastore
	wm iface.IWorldMgr   // world manager
	gm iface.IGameMgr    // game manager
	mp iface.IMsgParser  // msg parser

	gameSrv micro.Service

	rds      *redis.Client // redis
	tcpServ  *TCPServer    // tcp server
	httpServ *HttpServer   // http server
	wg       sync.WaitGroup
}

// NewUltimate return IUltimate
func NewUltimate() (iface.IUltimate, error) {
	umt := &ultimate{}

	if err := umt.initDatastore(); err != nil {
		return nil, err
	}

	if err := umt.initTask(); err != nil {
		return nil, err
	}

	if err := umt.initWorldMgr(); err != nil {
		return nil, err
	}

	if err := umt.initGameMgr(); err != nil {
		return nil, err
	}

	if err := umt.initMsgParser(); err != nil {
		return nil, err
	}

	if err := umt.initTCPServer(); err != nil {
		return nil, err
	}

	if err := umt.initHTTPServer(); err != nil {
		return nil, err
	}

	log.Info().Msg("all init ok!")

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
func (umt *ultimate) initTask() error {
	var err error
	if umt.td, err = task.NewDispatcher(); err != nil {
		return err
	}

	log.Info().Msg("task init ok!")

	return nil
}

// init datastore
func (umt *ultimate) initDatastore() error {
	var err error
	if umt.ds, err = datastore.NewDatastore(); err != nil {
		return err
	}

	log.Info().Msg("datastore init ok!")
	return nil
}

func (umt *ultimate) InitRedis() {
	umt.rds = redis.NewClient(&redis.Options{
		Addr:     global.RedisAddr,
		Password: global.RedisPwd,
		DB:       global.RedisDB,
	})

	if _, err := umt.rds.Ping().Result(); err != nil {
		log.Fatal().Err(err).Send()
		return
	}

	log.Info().Msg("redis init ok")
}

func (umt *ultimate) initMsgParser() error {
	umt.mp = NewMsgParser(umt.gm, umt.wm)
	log.Info().Msg("msg parser init ok!")
	return nil
}

// InitTCPServer init
func (umt *ultimate) initTCPServer() error {
	var err error
	if umt.tcpServ, err = NewTcpServer(umt.mp, umt.td); err != nil {
		return err
	}

	log.Info().Msg("tcp_server init ok!")
	return nil
}

// init http server
func (umt *ultimate) initHTTPServer() error {
	umt.httpServ = NewHttpServer(umt.gm)
	log.Info().Msg("http_server init ok!")
	return nil
}

// init world session
func (umt *ultimate) initWorldMgr() error {
	var err error
	if umt.wm, err = world.NewWorldMgr(umt.ds); err != nil {
		return err
	}

	log.Info().Msg("world_mgr init ok!")
	return nil
}

func (umt *ultimate) initGameMgr() error {

	// New Service
	umt.gameSrv = micro.NewService(
		micro.Name("ultimate-service-game"),
		micro.WrapHandler(prometheus.NewHandlerWrapper()),
		// micro.Version("latest"),
		// micro.Transport(transport.NewTransport()),
	)

	// init service
	umt.gameSrv.Init()

	// init game mgr
	var err error
	if umt.gm, err = game.NewGameMgr(umt.wm, umt.gameSrv); err != nil {
		return err
	}

	log.Info().Msg("game_mgr init ok!")

	return nil
}

// run
func (umt *ultimate) Run() {
	go umt.tcpServ.Run()
	go umt.httpServ.Run()
	go umt.wm.Run()
	go umt.gm.Run()
	go umt.ds.Run()

	// rpc service
	go func() {
		defer utils.CaptureException()
		if err := umt.gameSrv.Run(); err != nil {
			log.Fatal().Err(err).Send()
		}
	}()

	// server exit
	c := make(chan os.Signal, 1)
	signal.Notify(c, syscall.SIGHUP, syscall.SIGQUIT, syscall.SIGTERM, syscall.SIGINT)
	for {
		sig := <-c
		close(c)
		log.Info().Msgf("ultimate server closing down (signal: %v)", sig)

		switch sig {
		case syscall.SIGQUIT, syscall.SIGTERM, syscall.SIGSTOP, syscall.SIGINT:
			umt.Stop()
			log.Info().Msg("server exit safely")
			return
		case syscall.SIGHUP:
		default:
			return
		}
	}
}

func (umt *ultimate) Stop() {
	umt.td.Stop()
	umt.tcpServ.Stop()
	umt.ds.Stop()
	umt.wm.Stop()
}
