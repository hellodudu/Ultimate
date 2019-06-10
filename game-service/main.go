package main

import (
	"fmt"
	"log"
	"os"
	"os/signal"
	"syscall"
	"time"

	_ "github.com/go-sql-driver/mysql"
	"github.com/hellodudu/Ultimate/game-service/handler"
	ultimate "github.com/hellodudu/Ultimate/game-service/server"
	pbGame "github.com/hellodudu/Ultimate/proto/game"
	"github.com/hellodudu/data_mgr/subscriber"
	"github.com/micro/go-micro"
	log "github.com/sirupsen/logrus"
)

func main() {

	// log file
	t := time.Now()
	fileTime := fmt.Sprintf("%d-%d-%d %d-%d-%d", t.Year(), t.Month(), t.Day(), t.Hour(), t.Minute(), t.Second())
	logFn := fmt.Sprintf("log/%s_ultimate_service_game.log", fileTime)

	file, err := os.OpenFile(logFn, os.O_CREATE|os.O_WRONLY, 0666)
	if err == nil {
		log.SetOutput(file)
	} else {
		log.Info("Failed to log to file, using default stderr")
	}

	umt, err := ultimate.NewUltimate()
	if err != nil {
		log.Fatal(err)
	}

	umt.Run()

	// xmlloader
	// res.NewXmlLoader()

	// New Service
	service := micro.NewService(
		micro.Name("ultimate.service.srv.game-service"),
		micro.Version("latest"),
	)

	// Initialise service
	service.Init()

	// Register Handler
	pbGame.RegisterGameServiceHandler(service.Server(), new(handler.Example))

	// Register Struct as Subscriber
	micro.RegisterSubscriber("ultimate.service.srv.game-service", service.Server(), new(subscriber.Example))

	// Register Function as Subscriber
	micro.RegisterSubscriber("ultimate.service.srv.game-service", service.Server(), subscriber.Handler)

	// Run service
	if err := service.Run(); err != nil {
		log.Fatal(err)
	}

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
	os.Exit(0)
}
