package main

import (
	"log"
	"os"
	"os/signal"

	_ "github.com/go-sql-driver/mysql"
	"github.com/hellodudu/comment/res"
	"github.com/hellodudu/comment/server"
	"github.com/hellodudu/comment/utils"
)

var ultimateAPI *ultimate.UltimateAPI

func main() {
	var err error
	if ultimateAPI, err = ultimate.NewUltimateAPI(); err != nil {
		log.Fatal(err)
	}
	defer ultimateAPI.Close()

	ultimateAPI.Run()

	// http handle
	go func() {

	}()

	// test crc32
	log.Println("message crc32:", utils.Crc32(string("MWU_WorldLogon")))

	// xmlloader
	res.NewXmlLoader()

	// server exit
	c := make(chan os.Signal, 1)
	signal.Notify(c, os.Interrupt, os.Kill)
	sig := <-c
	log.Printf("ultimate server closing down (signal: %v)\n", sig)
}

func GetUltimateAPI() *ultimate.UltimateAPI {
	return ultimateAPI
}
