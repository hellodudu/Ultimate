package world_session

// base net message type define
type BaseNetMsg struct {
	Id   uint32 // message name crc32
	Size uint32 // message size
}

// world logon
type MSG_MWU_WorldLogon struct {
	BaseNetMsg
	WorldID   uint32
	WorldName [32]byte
}
