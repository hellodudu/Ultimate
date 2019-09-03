package main

import (
	"os"

	"github.com/hellodudu/Ultimate/game-service/server"

	_ "net/http/pprof"

	_ "github.com/go-sql-driver/mysql"
	logger "github.com/sirupsen/logrus"
)

func main() {

	umt, err := server.NewUltimate()
	if err != nil {
		logger.Fatal(err)
	}

	umt.Run()
	os.Exit(0)
}
