package main

import (
	"bufio"
	"encoding/json"
	"io/ioutil"
	"log"
	"net"
	"net/http"
	"os"
	"os/signal"
	"strings"

	_ "github.com/go-sql-driver/mysql"
	"github.com/golang/protobuf/proto"
	"github.com/gorilla/websocket"
	"github.com/hellodudu/comment/comt"
	"github.com/hellodudu/comment/proto"
	"github.com/hellodudu/comment/task"
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
		msg := scanner.Text()
		log.Printf("tcp recv:%s\n", msg)
		reply := strings.Join([]string{"server recv:", msg}, "")
		con.Write([]byte(reply))
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
		addr := string("127.0.0.1:8081")
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

	// server exit
	c := make(chan os.Signal, 1)
	signal.Notify(c, os.Interrupt, os.Kill)
	sig := <-c
	log.Printf("Leaf closing down (signal: %v)\n", sig)
}
