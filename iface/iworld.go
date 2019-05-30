package iface

import (
	"net"

	"github.com/golang/protobuf/proto"
)

type IWorldMgr interface {
	AddWorld(id uint32, name string, con net.Conn) (IWorld, error)
	AddWorldRef(id uint32, ref []uint32)
	BroadCast(msg proto.Message)
	DisconnectWorld(con net.Conn)
	GetWorldByCon(con net.Conn) IWorld
	GetWorldByID(id uint32) IWorld
	KickWorld(id uint32)
	Run()
	Stop() chan struct{}
}

type IWorld interface {
	GetID() uint32
	GetName() string
	SetLastConTime(t int)
	GetCon() net.Conn
	ResetTestConnect()
	Run()
	SendProtoMessage(p proto.Message)
	SendTransferMessage(data []byte)
	Stop()
}
