package main

import (
	"context"
	"fmt"
	"os"
	"os/signal"
	"syscall"

	"github.com/hellodudu/Ultimate/arena-service/arena"
	datastore "github.com/hellodudu/Ultimate/arena-service/db"
	"github.com/hellodudu/Ultimate/logger"
	pbArena "github.com/hellodudu/Ultimate/proto/arena"
	"github.com/hellodudu/Ultimate/utils/global"
	"github.com/micro/go-micro"
)

func main() {

	logger.Init(global.Debugging, "ultimate_service_arena")

	ds, err := datastore.NewDatastore()
	if err != nil {
		logger.Fatal(err)
	}

	// new arena
	arena, err := arena.NewArena(context.Background(), ds)
	if err != nil {
		logger.Fatal(err)
	}

	// New Service
	service := micro.NewService(
		micro.Name("ultimate.service.arena"),
		micro.Version("latest"),
	)

	// Initialise service
	service.Init()

	// Register Handler
	pbArena.RegisterArenaServiceHandler(service.Server(), arena)

	// Register Struct as Subscriber
	// micro.RegisterSubscriber("ultimate.service.arena", service.Server(), arena.SubHandler)

	// Run service
	go func() {
		if err := service.Run(); err != nil {
			logger.Fatal(err)
		}
	}()

	// Run datastore
	go ds.Run()

	// server exit
	c := make(chan os.Signal, 1)
	signal.Notify(c, syscall.SIGHUP, syscall.SIGQUIT, syscall.SIGTERM, syscall.SIGINT)
	for {
		sig := <-c
		logger.Info(fmt.Sprintf("ultimate server closing down (signal: %v)", sig))

		switch sig {
		case syscall.SIGQUIT, syscall.SIGTERM, syscall.SIGSTOP, syscall.SIGINT:
			arena.Stop()
			<-ds.Stop()
			logger.Info("server exit safely")
			return
		case syscall.SIGHUP:
		default:
			return
		}
	}
}
