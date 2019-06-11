package main

import (
	"fmt"
	"os"
	"time"

	"github.com/hellodudu/Ultimate/arena-service/handler"
	pbArena "github.com/hellodudu/Ultimate/proto/arena"
	"github.com/micro/go-micro"
	log "github.com/sirupsen/logrus"
)

func main() {
	// log file
	t := time.Now()
	fileTime := fmt.Sprintf("%d-%d-%d %d-%d-%d", t.Year(), t.Month(), t.Day(), t.Hour(), t.Minute(), t.Second())
	logFn := fmt.Sprintf("log/%s_ultimate_service_arena.log", fileTime)

	file, err := os.OpenFile(logFn, os.O_CREATE|os.O_WRONLY, 0666)
	if err == nil {
		log.SetOutput(file)
	} else {
		log.Info("Failed to log to file, using default stderr")
	}

	// New Service
	service := micro.NewService(
		micro.Name("ultimate.service.arena"),
		micro.Version("latest"),
	)

	// Initialise service
	service.Init()

	// arena
	h := handler.NewArenaHandler()

	// Register Handler
	pbArena.RegisterArenaServiceHandler(service.Server(), h)

	// Register Struct as Subscriber
	micro.RegisterSubscriber("ultimate.service.arena", service.Server(), h.SubHandler)

	// Run service
	if err := service.Run(); err != nil {
		log.Fatal(err)
	}
}
