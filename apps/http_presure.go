package main

import (
	"context"
	"os"
	"os/signal"
	"sync"
	"syscall"
	"time"

	"github.com/gammazero/workerpool"
	pbGame "github.com/hellodudu/Ultimate/proto/game"
	"github.com/hellodudu/Ultimate/utils"
	"github.com/micro/go-micro/v2"
	_ "github.com/micro/go-plugins/broker/nsq/v2"
	log "github.com/rs/zerolog/log"
)

var (
	testNum  = 10000
	address  = "localhost:7080"
	playerID = []int64{
		2820379275230707713,
		2820379275230707714,
		2820379275230707715,
	}

	wg sync.WaitGroup
)

func main() {

	service := micro.NewService(
		micro.Name("ultimate-service-client"),
		// micro.Version("latest"),
		// micro.Transport(transport.NewTransport()),
	)

	// Initialise service
	service.Init()

	// Run service
	go func() {
		defer utils.CaptureException()
		if err := service.Run(); err != nil {
			log.Fatal().Err(err).Send()
		}
	}()

	gameCli := pbGame.NewGameServiceClient(
		"ultimate-service-game",
		service.Client(),
	)

	// arenaCli := pbArena.NewArenaServiceClient(
	// 	"ultimate-service-game",
	// 	service.Client(),
	// )

	t := time.Now()
	wp := workerpool.New(4)
	for n := 0; n < testNum; n++ {
		wp.Submit(func() {
			_, err := gameCli.GetPlayerInfoByID(context.Background(), &pbGame.GetPlayerInfoByIDRequest{Id: playerID[0]})
			if err != nil {
				log.Warn().Err(err).Msg("GetArenaSeasonData Response")
				return
			}
		})
	}

	wp.StopWait()
	d := time.Since(t)

	// td, err := task.NewDispatcher()
	// if err != nil {
	// 	return
	// }

	// t := time.Now()
	// wg.Add(testNum)
	// for n := 0; n < testNum; n++ {
	// 	go func(n int) {
	// 		td.AddTask(&task.TaskReqInfo{ID: n, Con: nil, Data: nil, CB: callback})
	// 	}(n)
	// }
	// wg.Wait()
	// d := time.Since(t)

	log.Warn().Dur("duration", d).Msg("elapse time")

	// server exit
	c := make(chan os.Signal, 1)
	signal.Notify(c, syscall.SIGHUP, syscall.SIGQUIT, syscall.SIGTERM, syscall.SIGINT)
	for {
		sig := <-c
		log.Info().Msg("ultimate server closing down (signal: %v)", sig)

		switch sig {
		case syscall.SIGQUIT, syscall.SIGTERM, syscall.SIGSTOP, syscall.SIGINT:
			log.Info().Msg("exit safely")
			return
		case syscall.SIGHUP:
		default:
			return
		}
	}
}
