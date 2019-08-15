package main

import (
	"os"

	"github.com/hellodudu/Ultimate/game-service/server"

	_ "net/http/pprof"

	_ "github.com/go-sql-driver/mysql"
	"github.com/hellodudu/Ultimate/utils/global"
	logger "github.com/hellodudu/Ultimate/utils/log"
)

func main() {

	logger.Init(global.Debugging, true, "ultimate_service_game")

	umt, err := server.NewUltimate()
	if err != nil {
		logger.Fatal(err)
	}

	umt.Run()
	os.Exit(0)
}
