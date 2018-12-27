package ultimate

import (
	"bytes"
	"encoding/binary"
	"encoding/json"
	"io/ioutil"
	"log"
	"net/http"

	"github.com/hellodudu/comment/config"
	"github.com/hellodudu/comment/task"
	"github.com/hellodudu/comment/utils"
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

func callBackTask(ts task.Tasker) {
	log.Println("task callback with reqnum:", ts.GetReq())
	ts.Write([]byte("It is done!"))
	testChan <- 1

	// test proto
	// p := &tutorial.Person{
	// 	Id:    int32(1234),
	// 	Name:  "John Doe",
	// 	Email: "jdoe@example.com",
	// 	Phones: []*tutorial.Person_PhoneNumber{
	// 		{Number: "555-4321", Type: tutorial.Person_HOME},
	// 	},
	// }
	// book := &tutorial.AddressBook{}
	// book.People = append(book.People, p)
	// log.Println("unmashaled book = ", book)
	// out, err := proto.Marshal(book)
	// log.Println("mashaled book = ", out)
	// if err != nil {
	// 	log.Fatalln("Failed to encode address book:", err)
	// }

	// if err := ioutil.WriteFile("address_book", out, 0644); err != nil {
	// 	log.Fatalln("Failed to write address book:", err)
	// }
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

func binaryHandler(w http.ResponseWriter, r *http.Request) {
	msg := &MSG_MWU_WorldLogon{}
	arrayData := []byte{44, 0, 0, 0, 65, 81, 58, 14, 44, 0, 0, 0, 1, 0, 0, 0, 76, 111, 99, 97, 108, 83, 101, 114, 118, 101, 114, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}

	byData := make([]byte, binary.Size(msg))

	// discard top 4 bytes(message size)
	copy(byData, arrayData[4:])

	buf := &bytes.Buffer{}
	if _, err := buf.Write(byData); err != nil {
		log.Fatal(err)
	}

	// proto buff begin
	// byProto := byMsg[16:]
	// book := &tutorial.AddressBook{}
	// if err := proto.Unmarshal(byProto, book); err != nil {
	// 	log.Fatalln("Failed to parse address book:", err)
	// }

	// top 4 bytes messageid
	msgID := binary.LittleEndian.Uint32(buf.Bytes()[:4])
	if msgID == utils.Crc32(string("MWU_WorldLogon")) {
		if err := binary.Read(buf, binary.LittleEndian, msg); err != nil {
			log.Fatal(err)
		}
		log.Printf("world<id:%d, name:%s> logon!\n", msg.WorldID, msg.WorldName)
	}

	log.Printf("translate msg:%+v\n", msg)
}
