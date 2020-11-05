package main

import (
	"fmt"
	"os"
	"strings"

	"github.com/hellodudu/Ultimate/game-service/server"
	logger "github.com/hellodudu/Ultimate/utils/log"

	_ "net/http/pprof"

	_ "github.com/go-sql-driver/mysql"
	log "github.com/rs/zerolog/log"
)

func main() {
	// check path
	path, err := os.Getwd()
	if err != nil {
		fmt.Println(err)
		os.Exit(0)
	}

	if strings.Contains(path, "game-service") {
		os.Chdir("../")
		newPath, _ := os.Getwd()
		fmt.Println("change current path to project root path:", newPath)
	}

	logger.InitLogger("game-service")

	umt, err := server.NewUltimate()
	if err != nil {
		log.Fatal().Err(err).Send()
	}

	umt.Run()
	os.Exit(0)
}
