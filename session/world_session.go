package world_session

// base net message type define
type BaseNetMsg struct {
	Id   uint32     // message name crc32
	Size uint32     // message size
	Data [1024]byte // message data
}
