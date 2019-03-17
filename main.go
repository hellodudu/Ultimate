package main

import (
	"fmt"
	"os"
	"os/signal"

	_ "github.com/go-sql-driver/mysql"
	"github.com/hellodudu/Ultimate/logger"
	ultimate "github.com/hellodudu/Ultimate/server"
)

func main() {
	api, err := ultimate.NewAPI()
	if err != nil {
		logger.Fatal(err)
	}

	api.Run()

	// xmlloader
	// res.NewXmlLoader()

	// server exit
	c := make(chan os.Signal, 1)
	signal.Notify(c, os.Interrupt, os.Kill)
	sig := <-c
	logger.Print(fmt.Sprintf("ultimate server closing down (signal: %v)\n", sig))
	api.Stop()
	os.Exit(0)
}
