package main

import (
	"fmt"
	"log"
	"net/http"

	"github.com/hellodudu/comment/task"
)

var td task.TaskDispatcher
var ReqNum int = 1

func taskHandler(w http.ResponseWriter, r *http.Request) {
	tk := &task.Task{Req: ReqNum}
	ReqNum++
	td.AddTask(tk)
}

func main() {
	if ret := td.Init(); !ret {
		fmt.Println("task dispatcher init failed!")
	}

	http.HandleFunc("/", taskHandler)
	log.Fatal(http.ListenAndServe(":8080", nil))

}
