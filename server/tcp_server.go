package ultimate

import (
	"encoding/binary"
	"io"
	"net"
	"time"

	"github.com/hellodudu/Ultimate/global"
	"github.com/hellodudu/Ultimate/logger"
)

var tcpReadBufMax = 1024 * 100

type TcpServer struct {
}

func NewTcpServer() (*TcpServer, error) {
	return &TcpServer{}, nil
}

func (server *TcpServer) Run() {
	addr, err := global.IniMgr.GetIniValue("config/ultimate.ini", "listen", "TcpListenAddr")
	if err != nil {
		logger.Error("cannot read ini TcpListenAddr!")
		return
	}

	ln, err := net.Listen("tcp", addr)
	if err != nil {
		logger.Error(err)
	}
	defer ln.Close()

	logger.Info("tcp server listening at ", addr)

	for {
		con, err := ln.Accept()
		if nerr, ok := err.(net.Error); ok && nerr.Temporary() {
			time.Sleep(100 * time.Millisecond)
			continue
		}

		if err != nil {
			logger.Error(err)
		}

		go handleTCPConnection(con)

	}
}

func handleTCPConnection(conn net.Conn) {
	defer conn.Close()

	logger.Info("a new tcp connection!")
	conn.(*net.TCPConn).SetKeepAlive(true)
	conn.(*net.TCPConn).SetKeepAlivePeriod(30 * time.Second)

	for {
		// read len
		b := make([]byte, 4)
		if _, err := io.ReadFull(conn, b); err != nil {
			logger.Info("one client connection was shut down:", err)
			return
		}

		var msgLen uint32
		msgLen = binary.LittleEndian.Uint32(b)

		// check len
		if msgLen > uint32(tcpReadBufMax) {
			logger.Warning("tcp recv error:message too long")
			continue
		} else if msgLen < 4 {
			logger.Warning("tcp recv error:message too short")
			continue
		}

		// data
		msgData := make([]byte, msgLen)
		copy(msgData, b[:])
		if _, err := io.ReadFull(conn, msgData); err != nil {
			logger.Warning("tcp recv error:", err)
			continue
		}

		// handle message
		Instance().AddTask(func() {
			Instance().GetWorldSession().HandleMessage(conn, msgData)
		})
	}
}

// func handleTcpConnection(con net.Conn) {
// 	defer con.Close()

// 	con.(*net.TCPConn).SetKeepAlive(true)
// 	con.(*net.TCPConn).SetKeepAlivePeriod(30 * time.Second)
// 	scanner := bufio.NewScanner(con)

// 	// first 4 bytes represent tcp package size, split it
// 	scanner.Split(func(data []byte, atEOF bool) (advance int, token []byte, err error) {
// 		if atEOF {
// 			return advance, token, io.EOF
// 		}

// 		logger.Trace("tcp recv data length = ", len(data))

// 		if len(data) > 4 {
// 			length := uint32(0)
// 			binary.Read(bytes.NewReader(data[:4]), binary.LittleEndian, &length)
// 			logger.Trace("tcp actual data size:", length)
// 			if int(length)+4 <= len(data) {
// 				return int(length) + 4, data[:int(length)+4], nil
// 			}
// 		}
// 		return
// 	})

// 	for {
// 		ok := scanner.Scan()
// 		if ok {
// 			byMsg := scanner.Bytes()
// 			Instance().AddTask(func() {
// 				Instance().GetWorldSession().HandleMessage(con, byMsg)
// 			})
// 		} else if err := scanner.Err(); err != nil {
// 			logger.Warning("scan error:", err)
// 			// end of connection
// 			Instance().GetWorldSession().DisconnectWorld(con)
// 			break
// 		} else {
// 			logger.Info("one client connection was shut down!")
// 			break
// 		}
// 	}
// }
