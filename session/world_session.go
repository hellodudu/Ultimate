package world_session

// base net message type define
type BaseNetMsg struct {
	Size uint32    // message size
	Id   uint32    // message name crc32
	Data [128]byte // message data
}
