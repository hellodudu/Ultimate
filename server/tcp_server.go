package ultimate

import (
	"encoding/binary"
	"io"
	"net"
	"sync"
	"time"

	"github.com/hellodudu/Ultimate/global"
	"github.com/hellodudu/Ultimate/logger"
)

var tcpReadBufMax = 1024 * 100

type TcpServer struct {
	conns      map[net.Conn]struct{}
	ln         net.Listener
	mutexConns sync.Mutex
	wgConns    sync.WaitGroup
}

func NewTcpServer() (*TcpServer, error) {
	s := &TcpServer{
		conns: make(map[net.Conn]struct{}),
	}

	addr, err := global.IniMgr.GetIniValue("config/ultimate.ini", "listen", "TcpListenAddr")
	if err != nil {
		return nil, err
	}

	ln, err := net.Listen("tcp", addr)
	if err != nil {
		return nil, err
	}

	logger.Info("tcp listening at ", addr)

	s.ln = ln
	return s, nil
}

func (server *TcpServer) Run() {
	var tempDelay time.Duration
	for {
		conn, err := server.ln.Accept()
		if err != nil {
			if ne, ok := err.(net.Error); ok && ne.Temporary() {
				if tempDelay == 0 {
					tempDelay = 5 * time.Millisecond
				} else {
					tempDelay *= 2
				}
				if max := 1 * time.Second; tempDelay > max {
					tempDelay = max
				}
				logger.Warning("accept error: %v; retrying in %v", err, tempDelay)
				time.Sleep(tempDelay)
				continue
			}
			return
		}
		tempDelay = 0

		server.mutexConns.Lock()
		if len(server.conns) >= 5000 {
			server.mutexConns.Unlock()
			conn.Close()
			logger.Warning("too many connections")
			continue
		}
		server.conns[conn] = struct{}{}
		server.mutexConns.Unlock()

		server.wgConns.Add(1)

		go func() {
			handleTCPConnection(conn)

			server.mutexConns.Lock()
			delete(server.conns, conn)
			server.mutexConns.Unlock()

			server.wgConns.Done()
		}()
	}
}

func (server *TcpServer) Stop() {
	server.wgConns.Wait()
	server.ln.Close()

	server.mutexConns.Lock()
	for conn := range server.conns {
		conn.Close()
	}
	server.conns = nil
	server.mutexConns.Unlock()
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
