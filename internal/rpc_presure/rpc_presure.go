package rpc_presure

import (
	"context"
	"log"
	"sync"
	"time"

	"github.com/hellodudu/Ultimate/internal/utils"
	"github.com/micro/go-micro"

	pbArena "github.com/hellodudu/Ultimate/proto/arena"
	pbGame "github.com/hellodudu/Ultimate/proto/game"
	logger "github.com/sirupsen/logrus"
)

var playerID = []int64{
	1452692363393630209,
	1452692363393630210,
	1452692363393630211,
	1452692363393630212,
	1452692363393630213,
	1452692363393630214,
	1452692363393630215,
	1452692363393630216,
	1452692363393630217,
	1452692363393630218,
}

type RPCPresure struct {
	sync.RWMutex
	ctx       context.Context
	cancel    context.CancelFunc
	opts      *Options
	waitGroup utils.WaitGroupWrapper
	service   micro.Service
}

func New(opts *Options) (*RPCPresure, error) {
	r := &RPCPresure{
		opts: opts,
	}

	r.ctx, r.cancel = context.WithCancel(context.Background())

	r.initService()

	return r, nil
}

func (r *RPCPresure) initService() {
	r.service = micro.NewService(
		micro.Name("rpc_client"),
	)
	r.service.Init()

	go func() {
		if err := r.service.Run(); err != nil {
			logger.Fatal(err)
		}
	}()
}

// Main starts an instance of RPCPresure and returns an
// error if there was a problem starting up.
func (r *RPCPresure) Main() error {

	exitCh := make(chan error)
	var once sync.Once
	exitFunc := func(err error) {
		once.Do(func() {
			if err != nil {
				log.Fatal("RPCPresure Main() error:", err)
			}
			exitCh <- err
		})
	}

	r.waitGroup.Wrap(func() {
		exitFunc(r.work())
	})

	err := <-exitCh
	return err
}

func (r *RPCPresure) Exit() {
	r.cancel()
	r.waitGroup.Wait()
}

func (r *RPCPresure) work() error {

	gameSrv := pbGame.NewGameService(
		"ultimate-service-game",
		r.service.Client(),
	)

	arenaSrv := pbArena.NewArenaService(
		"ultimate-service-arena",
		r.service.Client(),
	)

	now := time.Now()
	times := 0

	// begin work until times count down
	for {
		select {
		case <-r.ctx.Done():
			return nil
		default:
		}

		if e := r.call(gameSrv, arenaSrv); e != nil {
			logger.Warn("call rpc failed:", e)
		}

		times++
		if times >= r.opts.RPCTimes {
			d := time.Since(now)
			logger.Info("do rpc call ", r.opts.RPCTimes, " times, cost time ", d)
			time.Sleep(time.Second - d)
			now = time.Now()
		}
	}
}

func (r *RPCPresure) call(gameSrv pbGame.GameService, arenaSrv pbArena.ArenaService) error {
	_, err := gameSrv.GetPlayerInfoByID(r.ctx, &pbGame.GetPlayerInfoByIDRequest{Id: playerID[0]})
	return err
}
