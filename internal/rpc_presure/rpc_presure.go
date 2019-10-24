package rpc_presure

import (
	"context"
	"fmt"
	"log"
	"sync"
	"time"

	"github.com/hellodudu/Ultimate/internal/utils"
	"github.com/micro/go-micro"
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
	services  []micro.Service
}

func New(opts *Options) (*RPCPresure, error) {
	r := &RPCPresure{
		opts:     opts,
		services: make([]micro.Service, 0),
	}

	r.ctx, r.cancel = context.WithCancel(context.Background())

	r.initService()

	return r, nil
}

func (r *RPCPresure) initService() {
	for n := 0; n < r.opts.Workers; n++ {
		s := micro.NewService(
			micro.Name(fmt.Sprintf("rpc_client%d", n+1)),
		)

		s.Init()
		r.services = append(r.services, s)
	}

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

	for n := 0; n < r.opts.Workers; n++ {
		r.waitGroup.Wrap(func() {
			exitFunc(r.work(n))
		})
	}

	err := <-exitCh
	return err
}

func (r *RPCPresure) Exit() {
	r.cancel()
	r.waitGroup.Wait()
}

func (r *RPCPresure) work(n int) error {

	// Run service
	ch := make(chan int, 1)
	go func() {
		if err := service.Run(); err != nil {
			logger.Fatal(err)
		}
		ch <- 1
	}()
	<-ch

	gameCli := pbGame.NewGameServiceClient(
		"ultimate-service-game",
		r.services[n].Client(),
	)

	arenaCli := pbArena.NewArenaServiceClient(
		"ultimate-service-arena",
		r.services[n].Client(),
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

		if e := r.call(n, gameCli, arenaCli); e != nil {
			logger.Warn("worker ", n, " call rpc failed:", e)
		}

		times++
		if times >= r.opts.RPCTimes {
			d := time.Since(now)
			logger.Info("worker ", n, " do rpc call ", r.opts.RPCTimes, " times, cost time ", d)
			time.Sleep(time.Second - d)
			now = time.Now()
		}
	}
}

func (r *RPCPresure) call(n int, gameCli micro.Client, arenaCli micro.Client) error {
	_, err := gameCli.GetPlayerInfoByID(r.ctx, &pbGame.GetPlayerInfoByIDRequest{Id: playerID[0]})
	return err
}
