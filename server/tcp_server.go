package ultimate

import (
	"bufio"
	"bytes"
	"encoding/binary"
	"log"
	"net"

	"github.com/hellodudu/comment/config"
	"github.com/hellodudu/comment/session"
)

type TcpServer struct {
}

func NewTcpServer() (*TcpServer, error) {
	return &TcpServer{}, nil
}

func (server *TcpServer) Run() {
	addr := config.TcpListenAddr
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
}

func handleTcpConnection(con net.Conn) {
	defer con.Close()
	scanner := bufio.NewScanner(con)
	for scanner.Scan() {
		byMsg := make([]byte, 128+8)
		copy(byMsg, scanner.Bytes())
		log.Printf("tcp recv bytes:%v\n", scanner.Bytes())

		buf := &bytes.Buffer{}
		if _, err := buf.Write(byMsg); err != nil {
			log.Fatal(err)
		}

		msg := &world_session.BaseNetMsg{}
		if err := binary.Read(buf, binary.LittleEndian, msg); err != nil {
			log.Fatal(err)
		}
		log.Printf("read buffer success:%v\n", msg)
		log.Printf("msg world_id = %d\n", binary.LittleEndian.Uint32(msg.Data[4:8]))
	}
}
