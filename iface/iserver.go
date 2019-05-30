package iface

type IMsgParser interface {
	ParserMessage(con ITCPConn, data []byte)
}
