package main

import (
	"fmt"
	"os"
	"time"

	"github.com/hellodudu/Ultimate/invite-service/handler"
	pbInvite "github.com/hellodudu/Ultimate/proto/invite"
	"github.com/micro/go-micro"
	log "github.com/sirupsen/logrus"
)

func main() {
	// log file
	t := time.Now()
	fileTime := fmt.Sprintf("%d-%d-%d %d-%d-%d", t.Year(), t.Month(), t.Day(), t.Hour(), t.Minute(), t.Second())
	logFn := fmt.Sprintf("log/%s_ultimate_service_invite.log", fileTime)

	file, err := os.OpenFile(logFn, os.O_CREATE|os.O_WRONLY, 0666)
	if err == nil {
		log.SetOutput(file)
	} else {
		log.Info("Failed to log to file, using default stderr")
	}

	// New Service
	service := micro.NewService(
		micro.Name("ultimate.service.invite"),
		micro.Version("latest"),
	)

	// Initialise service
	service.Init()

	// invite
	h := handler.NewInviteHandler()

	// Register Handler
	pbInvite.RegisterInviteServiceHandler(service.Server(), h)

	// Register Struct as Subscriber
	micro.RegisterSubscriber("ultimate.service.invite", service.Server(), h.SubHandler)

	// Run service
	if err := service.Run(); err != nil {
		log.Fatal(err)
	}
}
