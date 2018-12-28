package ultimate

import "net"

type World struct {
	Id   uint32   // world id
	Name string   // world name
	Con  net.Conn // connection
}
