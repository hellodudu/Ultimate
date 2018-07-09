package main

import (
	"log"
	"net/http"
)

var taskDispatcher TaskDispatcher
var reqNum int

func editHandler(w http.ResponseWriter, r *http.Request) {
	task := &Task{req: reqNum}
	reqNum++
	taskDispatcher.AddTask(task)
}

func main() {
	reqNum = 1
	taskDispatcher.Init()

	http.HandleFunc("/edit/", editHandler)
	log.Fatal(http.ListenAndServe(":8080", nil))
}
