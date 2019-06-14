package iface

import pbGame "github.com/hellodudu/Ultimate/proto/game"
import pbArena "github.com/hellodudu/Ultimate/proto/arena"

type IGameMgr interface {
	Invite() IInvite
	AddGuildInfo(i *pbGame.CrossGuildInfo)
	AddGuildInfoList(s []*pbGame.CrossGuildInfo)
	AddPlayerInfo(p *pbGame.CrossPlayerInfo)
	AddPlayerInfoList(s []*pbGame.CrossPlayerInfo)
	GetGuildInfoByID(id int64) *pbGame.CrossGuildInfo
	GetPlayerInfoByID(id int64) *pbGame.CrossPlayerInfo
	GetArenaSeasonData() (int32, int32)
	GetArenaChampion() ([]*pbArena.ArenaChampion, error)
	ArenaMatching(id int64)
	ArenaAddRecord(*pbArena.ArenaRecord)
	ArenaBattleResult(attackID int64, targetID int64, attackWin bool)
	ArenaGetRank(id int64, page int)
	Run()
}

// type IArena interface {
// 	AddRecord(rec *pbGame.ArenaRecord)
// 	BattleResult(attack int64, target int64, win bool)
// 	GetArenaDataNum() int
// 	GetChampion() []*pbGame.ArenaChampion
// 	GetDataByID(id int64) (interface{}, error) // ret: (*arenaData, error)
// 	GetMatchingList() []int64
// 	GetRankListByPage(page int) interface{} //ret: []*arenaData
// 	GetRecordByID(id int64) (*pbGame.ArenaRecord, error)
// 	GetRecordNum() int
// 	GetRecordReqList() map[int64]uint32
// 	Season() int
// 	SeasonEndTime() int
// 	WeekEnd()
// 	WeekEndTime() int
// 	Matching(playerID int64)
// 	RequestRank(id int64, page int32)
// 	Run()
// 	Stop()
// 	SaveChampion()
// }

type IInvite interface {
	AddInvite(newbieId int64, inviterId int64) int32
	CheckInviteResult(newbieId int64, inviterId int64, errorCode int32)
	InviteRecharge(newbieId int64, newbieName string, inviterId int64, diamondGift int32)
	Stop()
}
