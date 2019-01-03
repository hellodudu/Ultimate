package ultimate

import (
	"bufio"
	"bytes"
	"encoding/binary"
	"log"
	"net"

	"github.com/hellodudu/comment/config"
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

	// first 4 bytes represent tcp package size, split it
	scanner.Split(func(data []byte, atEOF bool) (advance int, token []byte, err error) {
		if !atEOF {
			if len(data) > 4 {
				length := uint32(0)
				binary.Read(bytes.NewReader(data[:4]), binary.LittleEndian, &length)
				if int(length)+4 <= len(data) {
					return int(length) + 4, data[:int(length)+4], nil
				}
			}
		}
		return
	})

	// proto recv
	for scanner.Scan() {
		GetUltimateAPI().AddTask(func() {
			GetUltimateAPI().GetWorldSession().HandleMessage(con, scanner.Bytes())
		})
	}

	// end of connection
	GetUltimateAPI().GetWorldSession().DisconnectWorld(con)
}
