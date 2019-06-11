package main

import (
	"fmt"
	"log"
	"os"
	"time"

	_ "github.com/go-sql-driver/mysql"
	"github.com/hellodudu/Ultimate/game-service/server"
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

	umt, err := server.NewUltimate()
	if err != nil {
		log.Fatal(err)
	}

	umt.Run()
	os.Exit(0)
}
