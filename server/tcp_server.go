package ultimate

import (
	"bufio"
	"bytes"
	"encoding/binary"
	"io"
	"log"
	"net"
	"time"

	"github.com/fatih/color"
	"github.com/hellodudu/Ultimate/config"
)

type TcpServer struct {
}

func NewTcpServer() (*TcpServer, error) {
	return &TcpServer{}, nil
}

func (server *TcpServer) Run() {
	addr, err := config.GetIniMgr().GetIniValue("ini/config.ini", "listen", "TcpListenAddr")
	if err != nil {
		log.Println(color.RedString("cannot read ini TcpListenAddr!"))
		return
	}

	ln, err := net.Listen("tcp", addr)
	if err != nil {
		log.Fatalln(err)
	}
	defer ln.Close()

	log.Println("tcp listening at ", addr)

	for {
		con, err := ln.Accept()
		if nerr, ok := err.(net.Error); ok && nerr.Temporary() {
			time.Sleep(100 * time.Millisecond)
			continue
		}

		if err != nil {
			log.Fatal(err)
		}

		log.Println(color.CyanString("a new tcp connection!"))
		go handleTcpConnection(con)
	}
}

func handleTcpConnection(con net.Conn) {
	defer con.Close()

	con.(*net.TCPConn).SetKeepAlive(true)
	con.(*net.TCPConn).SetKeepAlivePeriod(30 * time.Second)
	scanner := bufio.NewScanner(con)

	// first 4 bytes represent tcp package size, split it
	scanner.Split(func(data []byte, atEOF bool) (advance int, token []byte, err error) {
		if atEOF {
			return advance, token, io.EOF
		}

		if len(data) > 4 {
			length := uint32(0)
			binary.Read(bytes.NewReader(data[:4]), binary.LittleEndian, &length)
			if int(length)+4 <= len(data) {
				return int(length) + 4, data[:int(length)+4], nil
			}
		}
		return
	})

	for {
		ok := scanner.Scan()
		if ok {
			byMsg := scanner.Bytes()
			Instance().AddTask(func() {
				Instance().GetWorldSession().HandleMessage(con, byMsg)
			})
		} else if err := scanner.Err(); err != nil {
			log.Println(color.YellowString("scan error:%s", err.Error()))
			// end of connection
			Instance().GetWorldSession().DisconnectWorld(con)
			break
		} else {
			log.Println(color.CyanString("one client connection shut down!"))
			break
		}
	}
}
