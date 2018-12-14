package main

import (
	"encoding/json"
	"io/ioutil"
	"log"
	"net/http"

	_ "github.com/go-sql-driver/mysql"
	"github.com/hellodudu/comment/comt"
)

var comtAPI *comt.ComtAPI

func taskHandler(w http.ResponseWriter, r *http.Request) {
	comtAPI.AddTask()
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
