//+build ignore

package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"
)

var endPoint = "http://127.0.0.1:8080/arena_api_request_rank"
var reqArena struct {
	id   int64 `json:"id"`
	page int   `json:"page"`
}

func call() {
	r := &reqArena{
		id:   1412159966747296018,
		page: 0,
	}

	reqJson, err := json.Marshal(r)
	if err != nil {
		fmt.Println("marshal json err:", err)
	}

	req, err := http.NewRequest("GET", endPoint, bytes.NewBuffer(reqJson))
	req.Header.Set("X-Custom-Header", "")
	req.Header.Set("Content-Type", "application/json")

	client := &http.Client{}
	resp, err := client.Do(req)
	if err != nil {
		panic(err)
	}
	defer resp.Body.Close()

	fmt.Println("response Status:", resp.Status)
	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		w.Write([]byte(err.Error()))
		return
	}

	var req struct {
		ID   int64 `json:"id"`
		Page int   `json:"page"`
	}

	if err := json.Unmarshal(body, &req); err != nil {
		w.Write([]byte(err.Error()))
		return
	}

}

func main() {

	fmt.Println("URL:>", url)

}
