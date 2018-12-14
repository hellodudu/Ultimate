package main

import (
	"encoding/json"
	"io/ioutil"
	"log"
	"net/http"

	_ "github.com/go-sql-driver/mysql"
	"github.com/hellodudu/comment/comt"
	"github.com/hellodudu/comment/task"
)

var comtAPI *comt.ComtAPI

func callBack(ts *task.Task) {
	log.Println("task callback with reqnum:", ts.GetReq())
}

func taskHandler(w http.ResponseWriter, r *http.Request) {
	//test add task
	// newApp := &comt.App{AppID: 1, AppName: "testName"}

	comtAPI.AddTask(callBack)
	w.Write([]byte("It is done!"))
}

func createAppHandler(w http.ResponseWriter, r *http.Request) {

	body, err := ioutil.ReadAll(r.Body)
	if err != nil {
		log.Fatal(err)
	}

	newApp := &comt.App{}
	if err := json.Unmarshal(body, newApp); err != nil {
		log.Fatal(err)
	}

	// add app
	if err := comtAPI.AddNewApp(newApp); err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

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
	if comtAPI, err = comt.NewComtAPI(); err != nil {
		log.Fatal(err)
	}
	defer comtAPI.Close()

	http.HandleFunc("/create_app", createAppHandler)
	http.HandleFunc("/task", taskHandler)
	log.Fatal(http.ListenAndServe(":8080", nil))

}
