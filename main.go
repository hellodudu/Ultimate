package main

import (
	"log"
	"os"
	"os/signal"

	_ "github.com/go-sql-driver/mysql"
	"github.com/hellodudu/Ultimate/res"
	ultimate "github.com/hellodudu/Ultimate/server"
)

func main() {
	api, err := ultimate.NewAPI()
	if err != nil {
		log.Fatal(err)
	}

	api.Run()

	// xmlloader
	res.NewXmlLoader()

	// server exit
	c := make(chan os.Signal, 1)
	signal.Notify(c, os.Interrupt, os.Kill)
	sig := <-c
	log.Printf("ultimate server closing down (signal: %v)\n", sig)
	api.Stop()
}
