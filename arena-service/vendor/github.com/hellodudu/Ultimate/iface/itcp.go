package iface

type ITCPConn interface {
	Close()
	Write(b []byte) (n int, err error)
}
