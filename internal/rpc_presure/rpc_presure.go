package rpc_presure

import (
	"context"
	"fmt"
	"log"
	"math/rand"
	"sync"
	"time"

	"github.com/hellodudu/Ultimate/internal/utils"
	"github.com/micro/go-micro"

	pbArena "github.com/hellodudu/Ultimate/proto/arena"
	pbGame "github.com/hellodudu/Ultimate/proto/game"
	logger "github.com/sirupsen/logrus"
)

var (
	playerID = []int64{
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
	guildID = []int64{
		1452692354803695617,
	}
	maxPage = int32(1)
)

type mappingFunc func(game pbGame.GameService, arena pbArena.ArenaService) error

type RPCPresure struct {
	sync.RWMutex
	ctx       context.Context
	cancel    context.CancelFunc
	opts      *Options
	waitGroup utils.WaitGroupWrapper
	service   micro.Service
	randFuncs []mappingFunc
}

type protoRequest struct {
	Name  string `json:"name"`
	Value string `json:"value"`
}

func New(opts *Options) (*RPCPresure, error) {
	r := &RPCPresure{
		opts:      opts,
		randFuncs: make([]mappingFunc, 0),
	}

	r.ctx, r.cancel = context.WithCancel(context.Background())

	r.initService()
	r.initRandFuncs()

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

func (r *RPCPresure) initRandFuncs() {
	// game: get player info
	r.randFuncs = append(r.randFuncs, func(game pbGame.GameService, arena pbArena.ArenaService) error {
		_, err := game.GetPlayerInfoByID(r.ctx, &pbGame.GetPlayerInfoByIDRequest{Id: playerID[rand.Intn(len(playerID))]})
		return err
	})

	// game: get guild info
	r.randFuncs = append(r.randFuncs, func(game pbGame.GameService, arena pbArena.ArenaService) error {
		_, err := game.GetGuildInfoByID(r.ctx, &pbGame.GetGuildInfoByIDRequest{Id: guildID[rand.Intn(len(guildID))]})
		return err
	})

	// arena: get season data
	r.randFuncs = append(r.randFuncs, func(game pbGame.GameService, arena pbArena.ArenaService) error {
		_, err := arena.GetSeasonData(r.ctx, &pbArena.GetSeasonDataRequest{})
		return err
	})

	// arena: get champion
	r.randFuncs = append(r.randFuncs, func(game pbGame.GameService, arena pbArena.ArenaService) error {
		_, err := arena.GetChampion(r.ctx, &pbArena.GetChampionRequest{})
		return err
	})

	// arena: get rank
	r.randFuncs = append(r.randFuncs, func(game pbGame.GameService, arena pbArena.ArenaService) error {
		_, err := arena.GetRank(r.ctx, &pbArena.GetRankRequest{PlayerId: playerID[rand.Intn(len(playerID))], Page: rand.Int31n(maxPage)})
		return err
	})

	// arena: get arena data num
	r.randFuncs = append(r.randFuncs, func(game pbGame.GameService, arena pbArena.ArenaService) error {
		_, err := arena.GetArenaDataNum(r.ctx, &pbArena.GetArenaDataNumRequest{})
		return err
	})

	// arena: get record num
	r.randFuncs = append(r.randFuncs, func(game pbGame.GameService, arena pbArena.ArenaService) error {
		_, err := arena.GetRecordNum(r.ctx, &pbArena.GetRecordNumRequest{})
		return err
	})

	// arena: get matching list
	r.randFuncs = append(r.randFuncs, func(game pbGame.GameService, arena pbArena.ArenaService) error {
		_, err := arena.GetMatchingList(r.ctx, &pbArena.GetMatchingListRequest{})
		return err
	})

	// arena: get record req list
	r.randFuncs = append(r.randFuncs, func(game pbGame.GameService, arena pbArena.ArenaService) error {
		_, err := arena.GetRecordReqList(r.ctx, &pbArena.GetRecordReqListRequest{})
		return err
	})

	// arena: get record by id
	r.randFuncs = append(r.randFuncs, func(game pbGame.GameService, arena pbArena.ArenaService) error {
		_, err := arena.GetRecordByID(r.ctx, &pbArena.GetRecordByIDRequest{Id: playerID[rand.Intn(len(playerID))]})
		return err
	})

	// arena: get rank list by page
	r.randFuncs = append(r.randFuncs, func(game pbGame.GameService, arena pbArena.ArenaService) error {
		_, err := arena.GetRankListByPage(r.ctx, &pbArena.GetRankListByPageRequest{Page: rand.Int31n(maxPage)})
		return err
	})
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
			times = 0
		}
	}
}

func (r *RPCPresure) call(gameSrv pbGame.GameService, arenaSrv pbArena.ArenaService) error {

	f := r.randFuncs[rand.Intn(len(r.randFuncs))]
	if f == nil {
		return fmt.Errorf("random func error")
	}

	return f(gameSrv, arenaSrv)
}
