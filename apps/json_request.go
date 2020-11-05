//+build ignore

package main

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"math/rand"
	"net/http"
	"os"
	"os/signal"
	"syscall"
	"time"

	pbArena "github.com/hellodudu/Ultimate/proto/arena"
	"github.com/hellodudu/Ultimate/world"
	"github.com/rs/zerolog/log"
)

var tickSeconds int = 10
var chFault chan int

type reqArena struct {
	ID   int64 `json:"id"`
	Page int   `json:"page"`
}

func callArenaRank() {
	endPoint := "http://118.25.151.103:8088/arena_api_request_rank"
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

	respJSON := &pbArena.MUW_RequestArenaRank{}
	if err := json.Unmarshal(body, &respJSON); err != nil {
		fmt.Println("unmarshal resp json err:", err)
		return
	}

	log.Info().
		Str("status", resp.Status).
		Int64("player_id", respJSON.PlayerId)
	Int32("score", respJSON.Score).
		Int32("page", respJSON.Page).
		Int32("rank", respJSON.Rank).
		Msg("recv response")

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

func callArenaSync() {
	endPoint := "http://118.25.151.103:8088/arena_api_sync_season"

	req, err := http.NewRequest("GET", endPoint, http.NoBody)
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

	respJSON := make([]*world.TestSeasonSync, 0)

	if err := json.Unmarshal(body, &respJSON); err != nil {
		fmt.Println("unmarshal resp json err:", err)
		return
	}

	log.Warn().
		Int32("status", resp.StatusCode).
		Int32("world_len", len(respJSON)).
		Msg("recv arena season sync")

	if resp.StatusCode == http.StatusOK {
		return
	}

	for _, v := range respJSON {
		if v.Season != 6 {
			log.Warn().
				Uint32("world_id", v.WorldID).
				Int32("season", v.Season).
				Int32("season_end_time", v.SeasonEndTime).
				Send()

			chFault <- 1
			return
		}
	}
}

func runArenaRank(ctx context.Context) {

	for {
		select {
		case <-ctx.Done():
			return
		case <-chFault:
			return
		default:
			t := time.Now()
			callArenaRank()
			d := time.Since(t)
			time.Sleep(time.Second*time.Duration(tickSeconds) - d)
		}
	}
}

func runArenaSync(ctx context.Context) {

	for {
		select {
		case <-ctx.Done():
			return
		case <-chFault:
			return
		default:
			t := time.Now()
			callArenaSync()
			d := time.Since(t)
			time.Sleep(time.Second*time.Duration(tickSeconds) - d)
		}
	}
}

func main() {

	ctx, cancel := context.WithCancel(context.Background())
	go runArenaRank(ctx)
	go runArenaSync(ctx)

	// server exit
	c := make(chan os.Signal, 1)
	signal.Notify(c, syscall.SIGHUP, syscall.SIGQUIT, syscall.SIGTERM, syscall.SIGINT)
	for {
		sig := <-c
		fmt.Println(fmt.Sprintf("\njson_request closing down (signal: %v)", sig))

		switch sig {
		case syscall.SIGQUIT, syscall.SIGTERM, syscall.SIGSTOP, syscall.SIGINT:
			cancel()
			fmt.Println("close graceful")
			return
		case syscall.SIGHUP:
		default:
			return
		}
	}
	os.Exit(0)
}
