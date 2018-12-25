package main

import (
	"bufio"
	"encoding/binary"
	"encoding/json"
	"io/ioutil"
	"log"
	"net"
	"net/http"
	"os"
	"os/signal"

	_ "github.com/go-sql-driver/mysql"
	"github.com/golang/protobuf/proto"
	"github.com/gorilla/websocket"
	"github.com/hellodudu/comment/comt"
	"github.com/hellodudu/comment/proto"
	"github.com/hellodudu/comment/res"
	"github.com/hellodudu/comment/task"
	"github.com/hellodudu/comment/utils"
)

var comtAPI *comt.ComtAPI
var testChan chan interface{} = make(chan interface{}, 1)
var upgrader = websocket.Upgrader{
	ReadBufferSize:  1024,
	WriteBufferSize: 1024,
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
	comtAPI.AddHttpTask(w, r, callBackTask)
	<-testChan
	log.Printf("taskHandler over\n", w)
}

func wsHandler(w http.ResponseWriter, r *http.Request) {
	conn, err := upgrader.Upgrade(w, r, nil)
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

func handleTcpConnection(con net.Conn) {
	defer con.Close()
	scanner := bufio.NewScanner(con)
	for scanner.Scan() {
		byMsg := scanner.Bytes()
		log.Printf("tcp recv bytes:%v\n", scanner.Bytes())
		log.Printf("recv msg size:%d, msg_id:%x, msg_size:%d, world_id:%d, world_name:%s\n", binary.LittleEndian.Uint32(byMsg[:4]), binary.LittleEndian.Uint32(byMsg[4:8]), binary.LittleEndian.Uint32(byMsg[8:12]), binary.LittleEndian.Uint32(byMsg[12:16]), string(byMsg[16:]))
	}
}

func main() {
	var err error
	if comtAPI, err = comt.NewComtAPI(); err != nil {
		log.Fatal(err)
	}
	defer comtAPI.Close()

	// http handle
	go func() {
		http.HandleFunc("/create_app", createAppHandler)
		http.HandleFunc("/task", taskHandler)
		http.HandleFunc("/ws", wsHandler)
		log.Fatal(http.ListenAndServe(":8080", nil))
	}()

	// tcp handle
	go func() {
		addr := string("192.168.2.124:7030")
		ln, err := net.Listen("tcp", addr)
		if err != nil {
			log.Fatalln(err)
		}
		defer ln.Close()

		log.Println("tcp listening at ", addr)

		for {
			con, err := ln.Accept()
			if err != nil {
				log.Fatalln(err)
			}
			go handleTcpConnection(con)
		}
	}()

	// test crc32
	log.Println("message crc32:", utils.Crc32(string("MWU_WorldLogon")))

	// xmlloader
	res.NewXmlLoader()
	// log.Println("load entity_client ok! ", loader.EntityClientXml[0])

	// server exit
	c := make(chan os.Signal, 1)
	signal.Notify(c, os.Interrupt, os.Kill)
	sig := <-c
	log.Printf("Leaf closing down (signal: %v)\n", sig)
}
