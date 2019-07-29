package iface

import (
	"github.com/golang/protobuf/proto"
)

type IWorldMgr interface {
	AddWorld(id uint32, name string, con ITCPConn) (IWorld, error)
	AddWorldRef(id uint32, ref []uint32)
	BroadCast(msg proto.Message)
	DisconnectWorld(con ITCPConn)
	GetWorldByCon(con ITCPConn) IWorld
	GetWorldByID(id uint32) IWorld
	KickWorld(id uint32)
	Run()
	Stop()
}

type IWorld interface {
	GetID() uint32
	GetName() string
	SetLastConTime(t int)
	GetCon() ITCPConn
	ResetTestConnect()
	Run()
	SendProtoMessage(p proto.Message)
	SendTransferMessage(data []byte)
	Stop()
}
