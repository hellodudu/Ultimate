package main

import (
	"fmt"
	"log"
	"net/http"

	"github.com/hellodudu/comment/task"
)

var TD task.TaskDispatcher
var ReqNum int

func taskHandler(w http.ResponseWriter, r *http.Request) {
	tk := task.Task{Req: ReqNum}
	ReqNum++
	TD.AddTask(tk)
}

func main() {
	ReqNum = 1
	if ret := TD.Init(); !ret {
		fmt.Println("task dispatcher init failed!")
	}

	http.HandleFunc("/", taskHandler)
	log.Fatal(http.ListenAndServe(":8080", nil))

}
