package main

import (
	"os"

	_ "github.com/go-sql-driver/mysql"
	"github.com/hellodudu/Ultimate/game-service/server"
	"github.com/hellodudu/Ultimate/logger"
	"github.com/hellodudu/Ultimate/utils/global"
)

func main() {

	logger.Init(global.Debugging, "ultimate_service_game")

	umt, err := server.NewUltimate()
	if err != nil {
		logger.Fatal(err)
	}

	umt.Run()
	os.Exit(0)
}
