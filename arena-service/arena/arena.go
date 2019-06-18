package arena

import (
	"context"
	"fmt"
	"sort"
	"sync"
	"time"

	"github.com/golang/protobuf/proto"
	"github.com/hellodudu/Ultimate/iface"
	"github.com/hellodudu/Ultimate/logger"
	pbArena "github.com/hellodudu/Ultimate/proto/arena"
	"github.com/sirupsen/logrus"
)

var arenaMatchSectionNum = 8 // arena section num
var arenaRankNumPerPage = 10
var arenaSeasonDays = 4 * 7       // one season = 4 weeks
var arenaRequestNewRecordDays = 7 // every week request new player record
var arenaDefaultScore = 1000      // default arena score

// rankRecord sort interface
type rankArenaData struct {
	item   []*arenaData
	rwLock sync.RWMutex
}

func (s *rankArenaData) Sort() {
	s.rwLock.Lock()
	defer s.rwLock.Unlock()
	sort.Sort(s)
}

func (s *rankArenaData) Len() int {
	return len(s.item)
}

func (s *rankArenaData) Length() int {
	s.rwLock.RLock()
	defer s.rwLock.RUnlock()
	return len(s.item)
}

func (s *rankArenaData) Swap(a, b int) {
	s.item[a], s.item[b] = s.item[b], s.item[a]
}

func (s *rankArenaData) Less(a, b int) bool {
	if s.item[a].Score == s.item[b].Score {
		if s.item[a].ReachTime == s.item[b].ReachTime {
			return s.item[a].Playerid < s.item[b].Playerid
		}
		return s.item[a].ReachTime < s.item[b].ReachTime
	}
	return s.item[a].Score > s.item[b].Score
}

func (s *rankArenaData) Get(n int) *arenaData {
	s.rwLock.RLock()
	defer s.rwLock.RUnlock()
	return s.item[n]
}

func (s *rankArenaData) GetBottom() *arenaData {
	s.rwLock.RLock()
	defer s.rwLock.RUnlock()
	if s.Len() == 0 {
		return nil
	}
	return s.item[len(s.item)-1]
}

func (s *rankArenaData) GetIndexBefore100(d *arenaData) int {
	s.rwLock.RLock()
	defer s.rwLock.RUnlock()

	rank := -1
	for n := 0; n < len(s.item); n++ {
		if n >= 100 {
			break
		}

		if s.item[n].Playerid == d.Playerid {
			rank = n
			break
		}
	}

	return rank
}

func (s *rankArenaData) GetTop(top int) []*arenaData {
	s.rwLock.RLock()
	defer s.rwLock.RUnlock()

	l := make([]*arenaData, 0)
	for n := 0; n < len(s.item); n++ {
		if n >= top {
			break
		}

		l = append(l, s.item[n])
	}

	return l
}

func (s *rankArenaData) GetListByPage(page int) []*arenaData {
	l := make([]*arenaData, 0)

	s.rwLock.RLock()
	defer s.rwLock.RUnlock()

	for n := 0 + int(page)*arenaRankNumPerPage; n < 10+int(page)*arenaRankNumPerPage; n++ {
		if n >= s.Len() {
			break
		}

		l = append(l, s.item[n])
	}

	return l
}

func (s *rankArenaData) Add(v *arenaData) {
	s.rwLock.Lock()
	defer s.rwLock.Unlock()
	s.item = append(s.item, v)
}

// arena player data
type arenaData struct {
	Playerid   int64  `gorm:"type:bigint(20);primary_key;column:player_id;default:-1;not null" json:"player_id,omitempty"`
	Score      int32  `gorm:"type:int(10);column:score;default:0;not null" json:"score,omitempty"`
	ReachTime  uint32 `gorm:"type:int(10);column:reach_time;default:0;not null" json:"reach_time,omitempty"`
	LastTarget int64  `gorm:"type:bigint(20);column:last_target;default:-1;not null" json:"last_target,omitempty"` // target cannot be last one
}

func (arenaData) TableName() string {
	return "arena_player"
}

// champion data
type championData struct {
	Rank       int    `gorm:"type:smallint(5);primary_key;column:champion_rank;default:0;not null"`
	PlayerID   int64  `gorm:"type:bigint(20);column:player_id;default:-1;not null"`
	Score      int    `gorm:"type:int(10);column:score;default:0;not null"`
	Season     int    `gorm:"type:int(10);column:arena_season;default:0;not null"`
	PlayerName string `gorm:"type:varchar(32);column:player_name;default:'';not null"`
	ServerName string `gorm:"type:varchar(32);column:server_name;default:'';not null"`
	MasterID   int    `gorm:"type:int(10);column:master_id;default:1;not null"`
	FashionID  int    `gorm:"type:int(10);column:fashion_id;default:-1;not null"`
}

func (championData) TableName() string {
	return "arena_champion"
}

func getSectionIndexByScore(score int32) int32 {
	if score < 1200 {
		return 0
	} else if score < 1500 {
		return 1
	} else if score < 1800 {
		return 2
	} else if score < 2100 {
		return 3
	} else if score < 2500 {
		return 4
	} else if score < 3000 {
		return 5
	} else if score < 3600 {
		return 6
	} else {
		return 7
	}
}

func getDefaultScoreBySection(secIdx int32) int32 {
	var def int32 = 1000
	switch secIdx {
	case 0:
		def = 1000
	case 1:
		def = 1200
	case 2:
		def = 1500
	case 3:
		def = 1800
	case 4:
		def = 2100
	case 5:
		def = 2500
	case 6:
		def = 3000
	case 7:
		def = 3600
	default:
		def = 1000
	}
	return def
}

// Arena data
type Arena struct {
	ds           iface.IDatastore
	mapArenaData sync.Map      // all player's arena data
	arrRankArena rankArenaData // slice of arena record sorted with ArenaScore

	mapRecord    sync.Map   // all player's arena record
	arrMatchPool []sync.Map // 8 level match pool map[playerid]struct{}
	matchingList sync.Map   // list of matching waiting player map[playerid]struct{}

	mapRecordReq map[int64]int64 // map of init player request map[playerid]time.Now() : next request time
	muRecordReq  sync.Mutex

	chMatchWaitOK chan int64 // match wait player channel

	championList []*championData // top 3 champion data
	cpLock       sync.RWMutex    // champion read write lock

	ctx      context.Context
	cancel   context.CancelFunc
	chDBInit chan struct{}

	handler *ArenaHandler
}

// NewArena create new arena
func NewArena(ctx context.Context, ds iface.IDatastore) (*Arena, error) {
	arena := &Arena{
		ds:            ds,
		arrRankArena:  rankArenaData{item: make([]*arenaData, 0)},
		arrMatchPool:  make([]sync.Map, arenaMatchSectionNum),
		chMatchWaitOK: make(chan int64, 1000),
		mapRecordReq:  make(map[int64]int64),
		chDBInit:      make(chan struct{}, 1),
		championList:  make([]*championData, 0),
	}

	arena.ctx, arena.cancel = context.WithCancel(ctx)
	arena.handler = NewArenaHandler(arena.ctx, arena)

	go arena.loadFromDB()
	go arena.run()

	return arena, nil
}

func (arena *Arena) Handler() *ArenaHandler {
	return arena.handler
}

func (arena *Arena) getArenaDataNum() int {
	n := 0
	arena.mapArenaData.Range(func(_, _ interface{}) bool {
		n++
		return true
	})
	return n
}

func (arena *Arena) GetDataByID(id int64) (interface{}, error) {
	v, ok := arena.mapArenaData.Load(id)
	if !ok {
		return nil, fmt.Errorf("cannot find arena data with id %d", id)
	}

	return v.(*arenaData), nil
}

func (arena *Arena) getRecordNum() int {
	n := 0
	arena.mapRecord.Range(func(_, _ interface{}) bool {
		n++
		return true
	})
	return n
}

func (arena *Arena) getRecordByID(id int64) (*pbArena.ArenaRecord, error) {
	v, ok := arena.mapRecord.Load(id)
	if !ok {
		return nil, fmt.Errorf("cannot find arena record with id %d", id)
	}

	return v.(*pbArena.ArenaRecord), nil
}

func (arena *Arena) getMatchingList() []int64 {
	l := make([]int64, 0)
	arena.matchingList.Range(func(k, _ interface{}) bool {
		l = append(l, k.(int64))
		return true
	})
	return l
}

func (arena *Arena) GetRecordReqList() map[int64]int64 {
	return arena.mapRecordReq
}

func (arena *Arena) getRankListByPage(page int) []*arenaData {
	return arena.arrRankArena.GetListByPage(page)
}

// seasonEndTime get season end time
func (arena *Arena) seasonEndTime() int {
	return arena.ds.TableGlobal().ArenaSeasonEndTime
}

// season get season
func (arena *Arena) season() int {
	return arena.ds.TableGlobal().ArenaSeason
}

func (arena *Arena) WeekEndTime() int {
	return arena.ds.TableGlobal().ArenaWeekEndTime
}

func (arena *Arena) getChampion() []*pbArena.ArenaChampion {
	arena.cpLock.RLock()
	defer arena.cpLock.RUnlock()

	championRet := make([]*pbArena.ArenaChampion, 0)
	for _, v := range arena.championList {
		champion := &pbArena.ArenaChampion{
			Rank:       int32(v.Rank) + 1,
			PlayerId:   v.PlayerID,
			Score:      int32(v.Score),
			PlayerName: v.PlayerName,
			ServerName: v.ServerName,
			MasterId:   uint32(v.MasterID),
			FashionId:  int32(v.FashionID),
		}

		championRet = append(championRet, champion)
	}

	return championRet
}

// Stop stop
func (arena *Arena) Stop() {
	arena.cancel()
}

func (arena *Arena) run() {
	<-arena.chDBInit

	for {
		select {
		// context canceled
		case <-arena.ctx.Done():
			logger.Info("arena context done!")
			return

		// matching request
		case id := <-arena.chMatchWaitOK:
			ok, err := arena.updateMatching(id)
			if err != nil {
				continue
			}

			if !ok {
				// try again 5 seconds later
				t := time.NewTimer(5 * time.Second)
				logger.Info("player:", id, " will retry matching in 5 seconds")
				go func(p int64) {
					<-t.C
					arena.chMatchWaitOK <- p
				}(id)
			}

		default:
			t := time.Now()
			arena.updateRequestRecord()
			arena.updateMatchingList()
			arena.updateTime()
			d := time.Since(t)
			time.Sleep(time.Millisecond - d)

		}
	}
}

// loadFromDB load arena data from db
func (arena *Arena) loadFromDB() {

	// load from arena_champion
	arena.ds.DB().Set("gorm:table_options", "ENGINE=InnoDB DEFAULT CHARSET=utf8mb4").AutoMigrate(championData{})
	arena.ds.DB().Find(&arena.championList)

	// load from arena_player
	var arenaDataList []*arenaData
	arena.ds.DB().Set("gorm:table_options", "ENGINE=InnoDB DEFAULT CHARSET=utf8mb4").AutoMigrate(arenaData{})
	arena.ds.DB().Find(&arenaDataList)

	arena.muRecordReq.Lock()
	for _, v := range arenaDataList {
		arena.mapArenaData.Store(v.Playerid, v)

		// add to record request list, delay 20s to start request
		arena.mapRecordReq[v.Playerid] = time.Now().Add(time.Second * 20).Unix()
	}
	arena.muRecordReq.Unlock()

	now := int(time.Now().Unix())

	// if arena weekEndTime was expired, set a new time one week later
	if arena.WeekEndTime() == 0 {
		arena.weekEnd()

		// if weekEndTime has gone past, call weekEnd() 9 minutes later(wait for all world connected)
	} else if now > arena.WeekEndTime() {
		t := time.NewTimer(9 * time.Minute)
		go func(ct *time.Timer) {
			<-ct.C
			arena.weekEnd()
		}(t)
	}

	// if arena seasonEndTime was expired, set a new seasonEndTime one season later
	if arena.seasonEndTime() == 0 {
		arena.nextSeason()

		// if season end time has gone past, call seasonEnd() 10 minutes later(wait for all world connected)
	} else if now > arena.seasonEndTime() {
		t := time.NewTimer(10 * time.Minute)
		go func(ct *time.Timer) {
			<-ct.C
			arena.seasonEnd()
		}(t)
	}

	// all init ok
	arena.chDBInit <- struct{}{}
}

func (arena *Arena) updateMatching(id int64) (bool, error) {

	// get player arena data
	d, ok := arena.mapArenaData.Load(id)
	if !ok {
		logger.WithFieldsWarn("cannot find player's arena data", logrus.Fields{
			"player_id": id,
		})
		return false, fmt.Errorf("cannot find player %d 's arena data", id)
	}

	data := d.(*arenaData)

	// function of find target
	f := func(sec int32) *pbArena.ArenaRecord {
		var r *pbArena.ArenaRecord
		arena.arrMatchPool[sec].Range(func(k, _ interface{}) bool {
			key := k.(int64)

			// cannot be self
			if key == id {
				return true
			}

			// cannot be last target
			if key == data.LastTarget {
				return true
			}

			dv, ok := arena.mapRecord.Load(key)
			if !ok {
				return true
			}

			r = dv.(*pbArena.ArenaRecord)
			return false
		})
		return r
	}

	// find in same section
	secIdx := getSectionIndexByScore(data.Score)
	dstRec := f(secIdx)

	// find in below section
	if dstRec == nil {
		for n := secIdx - 1; n >= 0; n-- {
			dstRec = f(n)
			if dstRec != nil {
				break
			}
		}
	}

	resp, err := arena.handler.GetPlayerInfoByID(id)
	if err != nil {
		logger.WithFieldsWarn("cannot find player's info", logrus.Fields{
			"player_id": id,
			"error":     err,
		})
		return false, nil
	}

	// send to world
	msg := &pbArena.MUW_ArenaStartBattle{
		AttackId: id,
	}

	if dstRec == nil {
		msg.Bot = true
	} else {
		msg.TargetRecord = dstRec
	}

	arena.sendWorldMessage(resp.Info.ServerId, msg)
	return true, nil
}

func (arena *Arena) updateRequestRecord() {
	arena.muRecordReq.Lock()
	defer arena.muRecordReq.Unlock()

	for id, t := range arena.mapRecordReq {
		// it's not right time to send request
		if time.Now().Unix() < t {
			continue
		}

		resp, err := arena.handler.GetPlayerInfoByID(id)
		logger.WithFieldsInfo("GetPlayerInfoByID Result", logrus.Fields{
			"response": resp,
			"error":    err,
		})

		// add 3 seconds interval
		if err != nil {
			arena.mapRecordReq[id] = time.Now().Add(time.Second * 3).Unix()
			continue
		}

		// send request and try again 1 minutes later
		msg := &pbArena.MUW_ArenaAddRecord{
			PlayerId: id,
		}

		err = arena.sendWorldMessage(resp.Info.ServerId, msg)
		if err != nil {
			continue
		}

		delete(arena.mapRecordReq, id)
	}
}

func (arena *Arena) updateMatchingList() {
	arena.matchingList.Range(func(k, _ interface{}) bool {
		key := k.(int64)

		if _, ok := arena.mapRecord.Load(key); ok {
			arena.chMatchWaitOK <- key
			arena.matchingList.Delete(key)
		}
		return true
	})
}

func (arena *Arena) updateTime() {
	t := int(time.Now().Unix())

	// week end
	if t >= arena.WeekEndTime() {
		arena.weekEnd()
	}

	// season end
	if t >= arena.seasonEndTime() {
		arena.seasonEnd()
	}
}

func (arena *Arena) sendWorldMessage(id uint32, m proto.Message) error {
	out, err := proto.Marshal(m)
	if err != nil {
		logger.Warn("proto marshal failed:", err)
		return err
	}

	if _, err := arena.handler.SendWorldMessage(id, proto.MessageName(m), out); err != nil {
		logger.Warn("sendWorldMessage reply err:", err)
		return err
	}

	return nil
}

func (arena *Arena) broadCast(m proto.Message) error {
	out, err := proto.Marshal(m)
	if err != nil {
		logger.WithFieldsWarn("proto marshal failed", logrus.Fields{
			"error": err,
		})
		return err
	}

	if _, err := arena.handler.BroadCast(proto.MessageName(m), out); err != nil {
		logger.Warn("broadcast reply err:", err)
		return err
	}

	return nil
}

// every monday request new player record and send weekly reward
func (arena *Arena) weekEnd() {
	// current time
	ct := time.Now()

	// current weekday
	cw := time.Now().Weekday()
	if cw == 0 {
		cw = 7
	}

	// one full week duration
	d := time.Duration(time.Hour) * time.Duration(24) * time.Duration(arenaRequestNewRecordDays)

	// now elapse duration
	e := time.Hour*time.Duration(24)*time.Duration(cw-1) + time.Hour*time.Duration(ct.Hour()) + time.Minute*time.Duration(ct.Minute()) + time.Second*time.Duration(ct.Second())

	// add 10 seconds inaccuracy
	arena.ds.TableGlobal().ArenaWeekEndTime = int(ct.Add(d - e + time.Duration(time.Second)*10).Unix())

	arena.ds.DB().Model(arena.ds.TableGlobal()).Updates(iface.TableGlobal{
		ArenaWeekEndTime: arena.ds.TableGlobal().ArenaWeekEndTime,
		TimeStamp:        int(time.Now().Unix()),
	})

	// every monday will request players new record
	// send request with time delay, 50 request per second
	var arrReq []int64
	arena.mapRecord.Range(func(k, _ interface{}) bool {
		id := k.(int64)
		arrReq = append(arrReq, id)
		return true
	})

	arena.muRecordReq.Lock()
	for k, v := range arrReq {
		_, err := arena.handler.GetPlayerInfoByID(v)
		if err != nil {
			continue
		}

		arena.mapRecordReq[v] = ct.Add(time.Second * time.Duration(k/50)).Unix()
	}
	arena.muRecordReq.Unlock()

	// send weekly reward
	type rewardWeek struct {
		id int64 // player_id
		s  int32 // score
	}

	// map[world_id]arrayRewardWeek
	mapWeekReward := make(map[uint32]([]*rewardWeek))
	arena.mapArenaData.Range(func(_, v interface{}) bool {
		data := v.(*arenaData)

		r := &rewardWeek{
			id: data.Playerid,
			s:  data.Score,
		}

		resp, err := arena.handler.GetPlayerInfoByID(r.id)
		if err != nil {
			return true
		}

		if _, ok := mapWeekReward[resp.Info.ServerId]; !ok {
			mapWeekReward[resp.Info.ServerId] = make([]*rewardWeek, 0)
		}

		mapWeekReward[resp.Info.ServerId] = append(mapWeekReward[resp.Info.ServerId], r)
		return true
	})

	// send to world
	for worldID, rewardList := range mapWeekReward {
		msg := &pbArena.MUW_ArenaWeeklyReward{
			Data: make([]*pbArena.ArenaWeeklyReward, 0),
		}

		for _, v := range rewardList {
			d := &pbArena.ArenaWeeklyReward{
				PlayerId: v.id,
				Score:    v.s,
			}

			msg.Data = append(msg.Data, d)
		}

		arena.sendWorldMessage(worldID, msg)
	}
}

func (arena *Arena) nextSeason() {
	// current time
	ct := time.Now()

	// current weekday
	cw := time.Now().Weekday()
	if cw == 0 {
		cw = 7
	}

	// one full season duration
	d := time.Duration(time.Hour) * time.Duration(24) * time.Duration(arenaSeasonDays)

	// now elapse duration
	e := time.Hour*time.Duration(24)*time.Duration(cw-1) + time.Hour*time.Duration(ct.Hour()) + time.Minute*time.Duration(ct.Minute()) + time.Second*time.Duration(ct.Second())

	// add 30 seconds inaccuracy
	arena.ds.TableGlobal().ArenaSeasonEndTime = int(ct.Add(d - e + time.Duration(time.Second)*30).Unix())
	arena.ds.TableGlobal().ArenaSeason++

	// save to db
	arena.ds.DB().Model(arena.ds.TableGlobal()).Updates(iface.TableGlobal{
		ArenaSeasonEndTime: arena.seasonEndTime(),
		ArenaSeason:        arena.season(),
		TimeStamp:          int(time.Now().Unix()),
	})

	// broadcast to all world
	msg := &pbArena.MUW_SyncArenaSeason{
		Season:  int32(arena.season()),
		EndTime: uint32(arena.seasonEndTime()),
	}
	arena.broadCast(msg)
}

func (arena *Arena) newSeasonRank() {
	// people who's section > 4, set score to 4 section default score
	// people who's section <= 4, set score to section - 1 default score
	arena.mapArenaData.Range(func(k, v interface{}) bool {
		value := v.(*arenaData)
		oldSec := getSectionIndexByScore(value.Score)

		newSec := func(s int32) int32 {
			if s > 4 {
				return 4
			}

			if s == 0 {
				return 0
			}

			return s - 1
		}(oldSec)

		newScore := getDefaultScoreBySection(newSec)
		value.Score = newScore

		// save to db
		go func(data *arenaData) {
			arena.ds.DB().Save(data)
		}(value)

		if oldSec != newSec {
			arena.arrMatchPool[oldSec].Delete(value.Playerid)
			arena.arrMatchPool[newSec].Store(value.Playerid, struct{}{})
		}
		return true
	})

	arena.arrRankArena.Sort()
}

// save top 3 champion id
func (arena *Arena) saveChampion() {
	list := arena.arrRankArena.GetTop(3)

	arena.cpLock.Lock()
	arena.championList = nil

	for k, v := range list {
		data := &championData{
			Rank:       k,
			PlayerID:   v.Playerid,
			Score:      int(v.Score),
			Season:     arena.season(),
			PlayerName: "",
			ServerName: "",
			MasterID:   1,
			FashionID:  -1,
		}

		rec, _ := arena.getRecordByID(v.Playerid)
		if rec != nil {
			data.PlayerName = rec.FirstGroup.Name
			data.ServerName = rec.FirstGroup.WorldName
			for _, val := range rec.FirstGroup.HeroRecord {
				if val.EntityId > 0 && val.EntityId < 1000 {
					data.MasterID = int(val.EntityId)
					data.FashionID = int(val.FashionId)
					break
				}
			}
		}

		arena.championList = append(arena.championList, data)
	}
	arena.cpLock.Unlock()

	// save to db
	arena.ds.DB().Delete(championData{})
	for _, v := range arena.championList {
		arena.ds.DB().Save(v)
	}

	// broadcast to all world
	msg := &pbArena.MUW_ArenaChampion{
		Data: arena.getChampion(),
	}

	arena.broadCast(msg)
}

// send top 100 reward mail
func (arena *Arena) seasonReward() {
	list := arena.arrRankArena.GetTop(100)

	for n, data := range list {
		resp, err := arena.handler.GetPlayerInfoByID(data.Playerid)
		if err != nil {
			logger.WithFieldsWarn("season reward cannot find player", logrus.Fields{
				"top":       n + 1,
				"player_id": data.Playerid,
				"error":     err,
			})
			continue
		}

		msg := &pbArena.MUW_ArenaSeasonReward{
			PlayerId: data.Playerid,
			Rank:     int32(n + 1),
		}

		arena.sendWorldMessage(resp.Info.ServerId, msg)
	}
}

func (arena *Arena) seasonEnd() {
	arena.saveChampion()
	arena.seasonReward()
	arena.nextSeason()
	arena.newSeasonRank()
}

func (arena *Arena) matching(playerID int64) {
	arena.muRecordReq.Lock()
	defer arena.muRecordReq.Unlock()

	_, ok := arena.mapRecord.Load(playerID)

	if ok {
		// add to match request
		arena.chMatchWaitOK <- playerID

		// request newlest record after 5 seconds
		arena.mapRecordReq[playerID] = time.Now().Add(time.Second * 5).Unix()

	} else {
		// request record
		arena.mapRecordReq[playerID] = time.Now().Unix()

		// add to matching wait list
		arena.matchingList.Store(playerID, struct{}{})
	}
}

// addRecord if existing then replace record
func (arena *Arena) addRecord(rec *pbArena.ArenaRecord) {

	if _, ok := arena.mapRecord.Load(rec.PlayerId); ok {
		// update record
		arena.mapRecord.Store(rec.PlayerId, rec)
	} else {
		// add new record and arena data
		data := &arenaData{
			Playerid:   rec.PlayerId,
			Score:      int32(arenaDefaultScore),
			ReachTime:  uint32(time.Now().Unix()),
			LastTarget: -1,
		}

		// use exist arena data first
		if s, ok := arena.mapArenaData.Load(rec.PlayerId); ok {
			d := s.(*arenaData)
			data.Score = d.Score
			data.ReachTime = d.ReachTime
			data.LastTarget = d.LastTarget
		}

		arena.mapArenaData.Store(rec.PlayerId, data)
		arena.mapRecord.Store(rec.PlayerId, rec)

		// add to slice record sorted by ArenaRecord
		arena.arrRankArena.Add(data)
		arena.arrRankArena.Sort()

		// add to matching cache
		index := getSectionIndexByScore(data.Score)
		arena.arrMatchPool[index].Store(rec.PlayerId, struct{}{})

		// save to db
		arena.ds.DB().Save(data)
	}
}

func (arena *Arena) reorderRecord(id int64, preSection, newSection int32) {
	arena.arrMatchPool[preSection].Delete(id)
	arena.arrMatchPool[newSection].Store(id, struct{}{})

}

// battleResult battle end
func (arena *Arena) battleResult(attack int64, target int64, win bool) {
	d, ok := arena.mapArenaData.Load(attack)
	if !ok {
		logger.WithFieldsWarn("cannot find attacker's arena data", logrus.Fields{
			"player_id": attack,
		})
		return
	}

	// record target into last target
	data := d.(*arenaData)

	if target > 0 {
		data.LastTarget = target
	}

	if win {
		// section change
		preSection := getSectionIndexByScore(data.Score)
		data.Score += 10
		data.ReachTime = uint32(time.Now().Unix())
		newSection := getSectionIndexByScore(data.Score)
		if preSection != newSection {
			arena.reorderRecord(attack, preSection, newSection)
		}

		// save to db
		arena.ds.DB().Save(data)

		// rank change
		arena.arrRankArena.Sort()
	}
}

// requestRank request rank by world, max page is 10
func (arena *Arena) requestRank(id int64, page int32) {
	if page >= 10 || page < 0 {
		logger.Warn("player ", id, " request rank error: page ", page)
		return
	}

	resp, err := arena.handler.GetPlayerInfoByID(id)
	if err != nil {
		logger.Warn("player ", id, " request rank error: cannot find player info ", err)
		return
	}

	msg := &pbArena.MUW_RequestArenaRank{
		PlayerId:      id,
		Page:          page,
		Score:         int32(arenaDefaultScore),
		Rank:          -1,
		SeasonEndTime: uint32(arena.seasonEndTime()),
		Infos:         make([]*pbArena.ArenaTargetInfo, 0),
	}

	// get player rank
	if d, ok := arena.mapArenaData.Load(id); ok {
		data := d.(*arenaData)
		msg.Score = data.Score
		msg.Rank = int32(arena.arrRankArena.GetIndexBefore100(data))
	}

	// rank player data
	l := arena.arrRankArena.GetListByPage(int(page))
	for _, r := range l {
		v, ok := arena.mapRecord.Load(r.Playerid)
		if !ok {
			continue
		}

		value := v.(*pbArena.ArenaRecord)

		info := &pbArena.ArenaTargetInfo{
			PlayerId:     value.PlayerId,
			PlayerName:   value.FirstGroup.Name,
			ServerName:   value.FirstGroup.WorldName,
			Level:        value.FirstGroup.Level,
			PlayerScore:  value.FirstGroup.PlayerScore,
			HeadProtrait: int32(value.FirstGroup.HeadProtrait),
			HeadQuality:  int32(value.FirstGroup.HeadQuality),
			ArenaScore:   r.Score,
		}
		msg.Infos = append(msg.Infos, info)
	}

	if msg.Page >= 10 {
		logger.WithFieldsWarn("reply arena request rank pages error", logrus.Fields{
			"page": msg.Page})
	}

	arena.sendWorldMessage(resp.Info.ServerId, msg)
}
