package main

import (
	"os"

	"github.com/hellodudu/Ultimate/game-service/server"
	logger "github.com/hellodudu/Ultimate/utils/log"
	micro_logger "github.com/micro/go-micro/v2/logger"

	_ "net/http/pprof"

	_ "github.com/go-sql-driver/mysql"
	log "github.com/rs/zerolog/log"
)

func main() {

	logger.InitLogger("game-service")
	micro_logger.Init(micro_logger.WithOutput(logger.Logger))

	umt, err := server.NewUltimate()
	if err != nil {
		log.Fatal().Err(err).Send()
	}

	umt.Run()
	os.Exit(0)
}
