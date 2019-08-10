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
	"github.com/hellodudu/Ultimate/utils/global"
	"github.com/micro/go-micro"
	"github.com/micro/go-micro/transport"
)

func main() {

	logger.Init(global.Debugging, true, "ultimate_service_arena")

	ds, err := datastore.NewDatastore()
	if err != nil {
		logger.Fatal(err)
	}

	// New Service
	service := micro.NewService(
		micro.Name("ultimate_service_arena"),
		micro.Version("latest"),
		micro.Transport(transport.NewTransport()),
	)

	// Initialise service
	service.Init()

	// new arena
	arena, err := arena.NewArena(context.Background(), service, ds)
	if err != nil {
		logger.Fatal(err)
	}

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
			ds.Stop()
			logger.Info("server exit safely")
			return
		case syscall.SIGHUP:
		default:
			return
		}
	}
}
