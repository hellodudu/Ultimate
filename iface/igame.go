package iface

import (
	pbArena "github.com/hellodudu/Ultimate/proto/arena"
	pbGame "github.com/hellodudu/Ultimate/proto/game"
)

type IGameMgr interface {
	Invite() IInvite
	AddGuildInfo(i *pbGame.CrossGuildInfo)
	AddGuildInfoList(s []*pbGame.CrossGuildInfo)
	AddPlayerInfo(p *pbGame.CrossPlayerInfo)
	AddPlayerInfoList(s []*pbGame.CrossPlayerInfo)
	GetGuildInfoByID(id int64) (*pbGame.CrossGuildInfo, error)
	GetPlayerInfoByID(id int64) (*pbGame.CrossPlayerInfo, error)
	GetArenaSeasonData() (int32, int32, error)
	GetArenaChampion() ([]*pbArena.ArenaChampion, error)
	GetArenaDataNum() int32
	GetArenaRecordNum() int32
	GetArenaMatchingList() ([]int64, error)
	GetArenaRecordReqList() ([]*pbArena.RecordReqList, error)
	GetArenaRecord(id int64) (*pbArena.ArenaRecord, error)
	GetArenaRankList(page int) ([]byte, error)
	ArenaAPIRequestRank(id int64, page int) ([]byte, error)
	ArenaSaveChampion() error
	ArenaWeekEnd() error
	ArenaMatching(id int64)
	ArenaAddRecord(*pbArena.ArenaRecord)
	ArenaBattleResult(attackID int64, targetID int64, attackWin bool)
	ArenaGetRank(id int64, page int32)
	Run()
}

type IInvite interface {
	AddInvite(newbieId int64, inviterId int64) int32
	CheckInviteResult(newbieId int64, inviterId int64, errorCode int32)
	InviteRecharge(newbieId int64, newbieName string, inviterId int64, diamondGift int32)
	Stop()
}
