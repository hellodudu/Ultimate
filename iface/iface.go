package iface

import (
	"database/sql"
	"net"

	"github.com/golang/protobuf/proto"
	"github.com/hellodudu/Ultimate/iface"
	pb "github.com/hellodudu/Ultimate/proto"
	"github.com/hellodudu/Ultimate/task"
)

type IUltimate interface {
	Run()
	Stop()
	WorldMgr() iface.IWorldMgr
	GameMgr() iface.IGameMgr
	DataStore() iface.IDataStore
}

type IDispatcher interface {
	AddTask(*task.TaskReqInfo)
}

type IDatastore interface {
	Exec(q string)
	Query(q string) (*sql.Rows, error)
	Run()
	Stop() chan struct{}
}

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
	ID() uint32
	Name() string
	ResetTestConnect()
	Run()
	SendProtoMessage(p proto.Message)
	SendTransferMessage(data []byte)
	Stop()
}

type IMsgParser interface {
	ParserMessage(con net.Conn, data []byte)
}

type IGameMgr interface {
	Arena() IArena
	Invite() IInvite
	AddGuildInfo(i *pb.CrossGuildInfo)
	AddGuildInfoList(s []*pb.CrossGuildInfo)
	AddPlayerInfo(p *pb.CrossPlayerInfo)
	AddPlayerInfoList(s []*pb.CrossPlayerInfo)
	GetGuildInfoByID(id int64) *pb.CrossGuildInfo
	GetPlayerInfoByID(id int64) *pb.CrossPlayerInfo
	Run()
}

type IArena interface {
	AddRecord(rec *pb.ArenaRecord)
	BattleResult(attack int64, target int64, win bool)
	GetArenaDataNum() int
	GetChampion() []*pb.ArenaChampion
	GetDataByID(id int64) (*arenaData, error)
	GetMatchingList() []int64
	GetRankListByPage(page int) []*arenaData
	GetRecordByID(id int64) (*pb.ArenaRecord, error)
	GetRecordNum() int
	GetRecordReqList() map[int64]uint32
	GetSeason() int
	GetSeasonEndTime() uint32
	LoadFromDB()
	Matching(playerID int64)
	RequestRank(id int64, page int32)
	Run()
	Stop()
}

type IInvite interface {
	AddInvite(newbieId int64, inviterId int64)
	CheckInviteResult(newbieId int64, inviterId int64, errorCode int32)
	InviteRecharge(newbieId int64, newbieName string, inviterId int64, diamondGift int32)
	Run()
	Stop()
}
