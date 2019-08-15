package main

import (
	"context"
	"fmt"
	"os"
	"os/signal"
	"sync"
	"syscall"
	"time"

	"github.com/gammazero/workerpool"
	pbGame "github.com/hellodudu/Ultimate/proto/game"
	"github.com/hellodudu/Ultimate/utils/global"
	logger "github.com/hellodudu/Ultimate/utils/log"
	"github.com/micro/go-micro"
	_ "github.com/micro/go-plugins/broker/nsq"
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

	logger.Init(global.Debugging, true, "http_presure")

	service := micro.NewService(
		micro.Name("ultimate-service-client"),
		// micro.Version("latest"),
		// micro.Transport(transport.NewTransport()),
	)

	// Initialise service
	service.Init()

	// Run service
	go func() {
		if err := service.Run(); err != nil {
			logger.Fatal(err)
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
				logger.WithFieldsWarn("GetArenaSeasonData Response", logger.Fields{
					"error": err,
				})
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

	logger.WithFieldsWarn("elapse time", logger.Fields{
		"duration": d,
	})

	// server exit
	c := make(chan os.Signal, 1)
	signal.Notify(c, syscall.SIGHUP, syscall.SIGQUIT, syscall.SIGTERM, syscall.SIGINT)
	for {
		sig := <-c
		logger.Info(fmt.Sprintf("ultimate server closing down (signal: %v)", sig))

		switch sig {
		case syscall.SIGQUIT, syscall.SIGTERM, syscall.SIGSTOP, syscall.SIGINT:
			logger.Info("exit safely")
			return
		case syscall.SIGHUP:
		default:
			return
		}
	}
}
