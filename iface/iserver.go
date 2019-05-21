package iface

import "net"

type IMsgParser interface {
	ParserMessage(con net.Conn, data []byte)
}
