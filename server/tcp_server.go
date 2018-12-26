package ultimate

import (
	"bufio"
	"bytes"
	"encoding/binary"
	"log"
	"net"

	"github.com/hellodudu/comment/config"
	"github.com/hellodudu/comment/session"
	"github.com/hellodudu/comment/utils"
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

	for scanner.Scan() {
		byScanMsg := scanner.Bytes()
		msg := &world_session.MSG_MWU_WorldLogon{}
		byData := make([]byte, binary.Size(msg))

		// discard top 4 bytes(message size)
		copy(byData, byScanMsg[4:])

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

		// get top 4 bytes messageid
		msgID := binary.LittleEndian.Uint32(buf.Bytes()[:4])
		if msgID == utils.Crc32(string("MWU_WorldLogon")) {
			if err := binary.Read(buf, binary.LittleEndian, msg); err != nil {
				log.Fatal(err)
			}
			log.Printf("world<id:%d, name:%s> logon!\n", msg.WorldID, msg.WorldName)
		}

		log.Printf("translate msg:%+v\n", msg)
	}
}
