package main

import (
	"fmt"
	"log"
	"net/http"

	"github.com/hellodudu/comment/comt"
	"github.com/hellodudu/comment/task"
)

var td *task.TaskDispatcher
var ReqNum int = 0
var appMap map[int]comt.App

func taskHandler(w http.ResponseWriter, r *http.Request) {
	ReqNum = ReqNum + 1
	tk := &task.Task{Req: ReqNum}
	td.AddTask(tk)

	w.Write([]byte("It is done!"))
}

func createAppHandler(w http.ResponseWriter, r *http.Request) {
	// todo check app exist

	newApp := comt.App{AppID: 1, PubKey: "pub_key", PriKey: "pri_key"}
	appMap[newApp.AppID] = newApp

	fmt.Printf("create new app %v!\n", appMap[newApp.AppID])

	w.Write([]byte("It is done!"))
}

func main() {
	var err error
	if td, err = task.NewTaskDispatcher(); err != nil {
		panic("new task dispatcher failed!")
	}

	appMap = make(map[int]comt.App)

	http.HandleFunc("/create_app", createAppHandler)
	http.HandleFunc("/task", taskHandler)
	log.Fatal(http.ListenAndServe(":8080", nil))

}
