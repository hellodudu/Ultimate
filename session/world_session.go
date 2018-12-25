package world_session

// base net message type define
type BaseNetMsg struct {
	Id   uint32    // message name crc32
	Size uint32    // message size
	Data [128]byte // message data
}
