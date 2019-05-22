package iface

import pb "github.com/hellodudu/Ultimate/proto"

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
	GetDataByID(id int64) (interface{}, error) // ret: (*arenaData, error)
	GetMatchingList() []int64
	GetRankListByPage(page int) interface{} //ret: []*arenaData
	GetRecordByID(id int64) (*pb.ArenaRecord, error)
	GetRecordNum() int
	GetRecordReqList() map[int64]uint32
	GetSeason() int
	GetSeasonEndTime() uint32
	Matching(playerID int64)
	RequestRank(id int64, page int32)
	Run()
	Stop()
}

type IInvite interface {
	AddInvite(newbieId int64, inviterId int64) int32
	CheckInviteResult(newbieId int64, inviterId int64, errorCode int32)
	InviteRecharge(newbieId int64, newbieName string, inviterId int64, diamondGift int32)
	Run()
	Stop()
}
