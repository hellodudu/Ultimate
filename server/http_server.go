package ultimate

import (
	"encoding/json"
	"io/ioutil"
	"log"
	"net/http"
	_ "net/http/pprof"

	"github.com/hellodudu/Ultimate/config"
)

var testChan chan interface{} = make(chan interface{}, 1)

type HttpServer struct {
}

func NewHttpServer() (*HttpServer, error) {
	return &HttpServer{}, nil
}

func (server *HttpServer) Run() {
	http.HandleFunc("/create_app", createAppHandler)
	http.HandleFunc("/task", taskHandler)
	http.HandleFunc("/ws", wsHandler)
	http.HandleFunc("/bn", binaryHandler)
	log.Fatal(http.ListenAndServe(config.HttpListenAddr, nil))
}

func taskHandler(w http.ResponseWriter, r *http.Request) {
	c := make(chan struct{}, 1)
	GetUltimateAPI().AddTask(func() {
		w.Write([]byte("taskHandler Callback!"))
		c <- struct{}{}
	})
	<-c
}

func wsHandler(w http.ResponseWriter, r *http.Request) {
	conn, err := config.Upgrader.Upgrade(w, r, nil)
	if err != nil {
		log.Println(err)
	}

	for {
		msgtype, p, err := conn.ReadMessage()
		if err != nil {
			log.Println(err)
			return
		}

		res := []byte("server recv:")
		if err := conn.WriteMessage(msgtype, append(res, p...)); err != nil {
			log.Println(err)
			return
		}
	}
}

func createAppHandler(w http.ResponseWriter, r *http.Request) {

	body, err := ioutil.ReadAll(r.Body)
	if err != nil {
		log.Fatal(err)
	}

	newApp := &App{}
	if err := json.Unmarshal(body, newApp); err != nil {
		log.Fatal(err)
	}

	// add app
	if err := GetUltimateAPI().AddNewApp(newApp); err != nil {
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

// for test
func binaryHandler(w http.ResponseWriter, r *http.Request) {
	data := []byte{58, 0, 0, 0, 94, 144, 225, 39, 10, 52, 10, 9, 104, 101, 108, 108, 111, 100, 117, 100, 117, 16, 161, 96, 26, 21, 104, 101, 108, 108, 111, 100, 117, 100, 117, 56, 54, 64, 103, 109, 97, 105, 108, 46, 99, 111, 109, 34, 13, 10, 11, 49, 51, 52, 48, 49, 48, 51, 57, 50, 57, 55}

	GetUltimateAPI().GetWorldSession().HandleMessage(nil, data)
}
