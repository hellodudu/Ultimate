//+build ignore

package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"math/rand"
	"net/http"
	"os"
	"os/signal"
	"syscall"
	"time"

	pb "github.com/hellodudu/Ultimate/proto"
	"github.com/sirupsen/logrus"
)

var endPoint = "http://118.25.151.103:8088/arena_api_request_rank"
var tickSeconds int = 3
var chFault chan int

type reqArena struct {
	ID   int64 `json:"id"`
	Page int   `json:"page"`
}

func call() {
	r := &reqArena{
		ID:   1412159966747296018,
		Page: rand.Intn(10),
	}

	reqJSON, err := json.Marshal(r)
	if err != nil {
		fmt.Println("marshal json err:", err)
		return
	}

	req, err := http.NewRequest("GET", endPoint, bytes.NewBuffer(reqJSON))
	req.Header.Set("X-Custom-Header", "")
	req.Header.Set("Content-Type", "application/json")

	client := &http.Client{}
	resp, err := client.Do(req)
	if err != nil {
		fmt.Println("do request err:", err)
		return
	}
	defer resp.Body.Close()

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		fmt.Println("read resp err:", err)
		return
	}

	respJSON := &pb.MUW_RequestArenaRank{}
	if err := json.Unmarshal(body, &respJSON); err != nil {
		fmt.Println("unmarshal resp json err:", err)
		return
	}

	logrus.WithFields(logrus.Fields{
		"status":    resp.Status,
		"player_id": respJSON.PlayerId,
		"score":     respJSON.Score,
		"page":      respJSON.Page,
		"rank":      respJSON.Rank,
	}).Info("recv respons")

	if resp.StatusCode == http.StatusOK {
		return
	}

	if respJSON.Page < 0 || respJSON.Page > 10 {
		chFault <- 1
		return
	}

	if respJSON.Score < 1000 || respJSON.Score > 5000 {
		chFault <- 1
		return
	}
}

func run() {

	for {
		select {
		case <-chFault:
			return
		default:
			t := time.Now()
			call()
			d := time.Since(t)
			time.Sleep(time.Second*time.Duration(tickSeconds) - d)
		}
	}
}

func main() {

	go run()

	// server exit
	c := make(chan os.Signal, 1)
	signal.Notify(c, syscall.SIGHUP, syscall.SIGQUIT, syscall.SIGTERM, syscall.SIGINT)
	for {
		sig := <-c
		fmt.Println(fmt.Sprintf("\njson_request closing down (signal: %v)", sig))

		switch sig {
		case syscall.SIGQUIT, syscall.SIGTERM, syscall.SIGSTOP, syscall.SIGINT:
			fmt.Println("close graceful")
			return
		case syscall.SIGHUP:
		default:
			return
		}
	}
	os.Exit(0)
}
