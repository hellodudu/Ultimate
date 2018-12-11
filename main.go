package main

import (
	"database/sql"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"

	_ "github.com/go-sql-driver/mysql"
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

	body, readErr := ioutil.ReadAll(r.Body)
	if readErr != nil {
		log.Fatal(readErr)
	}

	newApp := &comt.App{}
	if jsonErr := json.Unmarshal(body, newApp); jsonErr != nil {
		log.Fatal(jsonErr)
	}

	// appid exist
	if _, ok := appMap[newApp.AppID]; ok {
		retBuf := fmt.Sprintf("appid<%d> exist!\n", newApp.AppID)
		log.Println(retBuf)
		w.Write([]byte(retBuf))
		return
	}

	log.Printf("create new app %+v!\n", newApp)
	appMap[newApp.AppID] = *newApp

	retBuf, retErr := json.Marshal(newApp)
	if retErr != nil {
		log.Fatal("create app response json marshal error!")
	}

	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusOK)
	w.Write(retBuf)
}

func main() {
	var err error
	if td, err = task.NewTaskDispatcher(); err != nil {
		panic("new task dispatcher failed!")
	}

	db, err := sql.Open("mysql", "root:hello1986@tcp(127.0.0.1:3306)/comt")
	if err != nil {
		log.Fatal(err)
	}
	defer db.Close()
	fmt.Printf("mysql connect success %+v\n", db)

	appMap = make(map[int]comt.App)

	http.HandleFunc("/create_app", createAppHandler)
	http.HandleFunc("/task", taskHandler)
	log.Fatal(http.ListenAndServe(":8080", nil))

}
