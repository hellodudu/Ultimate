package main

import (
	"log"
	"os"
	"os/signal"

	_ "github.com/go-sql-driver/mysql"
	"github.com/hellodudu/Ultimate/res"
	"github.com/hellodudu/Ultimate/server"
)

func main() {
	ultimateAPI, err := ultimate.NewUltimateAPI()
	if err != nil {
		log.Fatal(err)
	}

	ultimateAPI.Run()

	// xmlloader
	res.NewXmlLoader()

	// server exit
	c := make(chan os.Signal, 1)
	signal.Notify(c, os.Interrupt, os.Kill)
	sig := <-c
	log.Printf("ultimate server closing down (signal: %v)\n", sig)
	ultimateAPI.Stop()
}
