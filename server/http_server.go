package ultimate

import (
	"encoding/json"
	"io/ioutil"
	"log"
	"net/http"

	"github.com/golang/protobuf/proto"
	"github.com/hellodudu/comment/config"
	"github.com/hellodudu/comment/proto"
	"github.com/hellodudu/comment/task"
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
	log.Fatal(http.ListenAndServe(config.HttpListenAddr, nil))
}

func callBackTask(ts task.Tasker) {
	log.Println("task callback with reqnum:", ts.GetReq())
	ts.Write([]byte("It is done!"))
	testChan <- 1

	// test proto
	p := &tutorial.Person{
		Id:    1234,
		Name:  "John Doe",
		Email: "jdoe@example.com",
		Phones: []*tutorial.Person_PhoneNumber{
			{Number: "555-4321", Type: tutorial.Person_HOME},
		},
	}
	book := &tutorial.AddressBook{}
	book.People = append(book.People, p)
	log.Println("unmashaled book = ", book)
	out, err := proto.Marshal(book)
	log.Println("mashaled book = ", out)
	if err != nil {
		log.Fatalln("Failed to encode address book:", err)
	}

	if err := ioutil.WriteFile("address_book", out, 0644); err != nil {
		log.Fatalln("Failed to write address book:", err)
	}
}

func taskHandler(w http.ResponseWriter, r *http.Request) {
	GetUltimateAPI().AddHttpTask(w, r, callBackTask)
	<-testChan
	log.Printf("taskHandler over\n", w)
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
