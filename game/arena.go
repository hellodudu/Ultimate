package game

import (
	"context"
	"fmt"
	"sort"
	"sync"
	"time"

	"github.com/hellodudu/Ultimate/iface"
	"github.com/hellodudu/Ultimate/logger"
	world_message "github.com/hellodudu/Ultimate/proto"
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
	Playerid   int64  `gorm:"type:bigint(20);primary_key;column:player_id;default:-1;not null"`
	Score      int32  `gorm:"type:int(10);column:score;default:0;not null"`
	ReachTime  uint32 `gorm:"type:int(10);column:reach_time;default:0;not null"`
	LastTarget int64  `gorm:"type:bigint(20);column:last_target;default:-1;not null"` // target cannot be last one
}

func (arenaData) TableName() string {
	return "arena_player"
}

// champion data
type championData struct {
	rank     int   `gorm:"type:smallint(5);primary_key;column:champion_rank;default:0;not null"`
	playerID int64 `gorm:"type:bigint(20);column:player_id;default:-1;not null"`
	score    int   `gorm:"type:int(10);column:score;default:0;not null"`
	season   int   `gorm:"type:int(10);column:arena_season;default:0;not null"`
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
	gm           iface.IGameMgr
	wm           iface.IWorldMgr
	ds           iface.IDatastore
	mapArenaData sync.Map      // all player's arena data
	arrRankArena rankArenaData // slice of arena record sorted with ArenaScore

	mapRecord    sync.Map   // all player's arena record
	arrMatchPool []sync.Map // 8 level match pool map[playerid]struct{}
	matchingList sync.Map   // list of matching waiting player map[playerid]struct{}

	mapRecordReq sync.Map // map of init player request map[playerid]time.Now() : next request time

	chMatchWaitOK chan int64 // match wait player channel

	championList []*championData // top 3 champion data
	cpLock       sync.RWMutex    // champion read write lock

	ctx      context.Context
	cancel   context.CancelFunc
	chDBInit chan struct{}
}

// NewArena create new arena
func NewArena(ctx context.Context, gm iface.IGameMgr, wm iface.IWorldMgr, ds iface.IDatastore) (iface.IArena, error) {
	arena := &Arena{
		gm:            gm,
		wm:            wm,
		ds:            ds,
		arrRankArena:  rankArenaData{item: make([]*arenaData, 0)},
		arrMatchPool:  make([]sync.Map, arenaMatchSectionNum),
		chMatchWaitOK: make(chan int64, 1000),
		chDBInit:      make(chan struct{}, 1),
		championList:  make([]*championData, 0),
	}

	arena.ctx, arena.cancel = context.WithCancel(ctx)
	go arena.loadFromDB()

	return arena, nil
}

func (arena *Arena) GetArenaDataNum() int {
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

func (arena *Arena) GetRecordNum() int {
	n := 0
	arena.mapRecord.Range(func(_, _ interface{}) bool {
		n++
		return true
	})
	return n
}

func (arena *Arena) GetRecordByID(id int64) (*world_message.ArenaRecord, error) {
	v, ok := arena.mapRecord.Load(id)
	if !ok {
		return nil, fmt.Errorf("cannot find arena record with id %d", id)
	}

	return v.(*world_message.ArenaRecord), nil
}

func (arena *Arena) GetMatchingList() []int64 {
	l := make([]int64, 0)
	arena.matchingList.Range(func(k, _ interface{}) bool {
		l = append(l, k.(int64))
		return true
	})
	return l
}

func (arena *Arena) GetRecordReqList() map[int64]uint32 {
	m := make(map[int64]uint32)
	arena.mapRecordReq.Range(func(k, v interface{}) bool {
		m[k.(int64)] = uint32(v.(time.Time).Unix())
		return true
	})
	return m
}

func (arena *Arena) GetRankListByPage(page int) interface{} {
	return arena.arrRankArena.GetListByPage(page)
}

// SeasonEndTime get season end time
func (arena *Arena) SeasonEndTime() int {
	return arena.ds.TableGlobal().ArenaSeasonEndTime
}

// Season get season
func (arena *Arena) Season() int {
	return arena.ds.TableGlobal().ArenaSeason
}

func (arena *Arena) WeekEndTime() int {
	return arena.ds.TableGlobal().ArenaWeekEndTime
}

func (arena *Arena) GetChampion() []*world_message.ArenaChampion {
	arena.cpLock.RLock()
	defer arena.cpLock.RUnlock()

	championRet := make([]*world_message.ArenaChampion, 0)
	for _, v := range arena.championList {
		champion := &world_message.ArenaChampion{
			Rank:     int32(v.rank) + 1,
			PlayerId: v.playerID,
			Score:    int32(v.score),
		}

		rec, err := arena.GetRecordByID(v.playerID)
		if err != nil {
			logger.Warning("GetChampion cannot get player's ArenaRecord:", v.playerID)
			continue
		}

		champion.PlayerName = rec.FirstGroup.Name
		champion.ServerName = rec.FirstGroup.WorldName
		for _, val := range rec.FirstGroup.HeroRecord {
			if val.EntityId > 0 && val.EntityId < 1000 {
				champion.MasterId = val.EntityId
				champion.FashionId = val.FashionId
				break
			}
		}

		championRet = append(championRet, champion)
	}

	return championRet
}

// Stop stop
func (arena *Arena) Stop() {
	arena.cancel()
}

// Run run
func (arena *Arena) Run() {
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
	arena.ds.DB().AutoMigrate(championData{})
	arena.ds.DB().Find(&arena.championList)

	// load from arena_player
	var arenaDataList []*arenaData
	arena.ds.DB().AutoMigrate(arenaData{})
	arena.ds.DB().Find(&arenaDataList)

	for _, v := range arenaDataList {
		arena.mapArenaData.Store(v.Playerid, v)

		// add to record request list, delay 20s to start request
		arena.mapRecordReq.Store(v.Playerid, time.Now().Add(time.Second*20))
	}

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
	if arena.SeasonEndTime() == 0 {
		arena.nextSeason()

		// if season end time has gone past, call seasonEnd() 10 minutes later(wait for all world connected)
	} else if now > arena.SeasonEndTime() {
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
		logger.Warning("cannot find player:", id, " 's arena data!")
		return false, fmt.Errorf("cannot find player %d 's arena data", id)
	}

	data := d.(*arenaData)

	// function of find target
	f := func(sec int32) *world_message.ArenaRecord {
		var r *world_message.ArenaRecord
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

			r = dv.(*world_message.ArenaRecord)
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

	info := arena.gm.GetPlayerInfoByID(id)
	if info == nil {
		logger.Warning("cannot find player ", id, " s info!")
		return false, nil
	}

	if world := arena.wm.GetWorldByID(info.ServerId); world != nil {
		msg := &world_message.MUW_ArenaStartBattle{
			AttackId: id,
		}

		if dstRec == nil {
			msg.Bot = true
		} else {
			msg.TargetRecord = dstRec
		}

		world.SendProtoMessage(msg)
	}

	return true, nil
}

func (arena *Arena) updateRequestRecord() {
	var arrDel []int64

	arena.mapRecordReq.Range(func(k, v interface{}) bool {
		id := k.(int64)
		t := v.(time.Time)

		// it's not right time to send request
		if time.Now().Unix() < t.Unix() {
			return true
		}

		info := arena.gm.GetPlayerInfoByID(id)
		if info == nil {
			return true
		}

		world := arena.wm.GetWorldByID(info.ServerId)
		if world == nil {
			return true
		}

		// send request and try again 1 minutes later
		msg := &world_message.MUW_ArenaAddRecord{
			PlayerId: id,
		}
		world.SendProtoMessage(msg)
		arrDel = append(arrDel, id)
		return true
	})

	for _, v := range arrDel {
		arena.mapRecordReq.Delete(v)
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
	if t >= arena.SeasonEndTime() {
		arena.seasonEnd()
	}
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

	// send request with time delay, 50 request per second
	var arrReq []int64
	arena.mapRecord.Range(func(k, _ interface{}) bool {
		id := k.(int64)
		arrReq = append(arrReq, id)
		return true
	})

	for k, v := range arrReq {
		info := arena.gm.GetPlayerInfoByID(v)
		if info == nil {
			continue
		}

		world := arena.wm.GetWorldByID(info.ServerId)
		if world == nil {
			continue
		}

		arena.mapRecordReq.Store(v, ct.Add(time.Second*time.Duration(k/50)))
	}

	// send weekly reward
	type rewardWeek struct {
		s int32
		t time.Time
	}

	mapReward := make(map[int64]*rewardWeek)
	var index int
	arena.mapArenaData.Range(func(_, v interface{}) bool {
		data := v.(*arenaData)

		r := &rewardWeek{}
		r.s = data.Score
		r.t = time.Now().Add(time.Second * time.Duration(2+index/50))

		mapReward[data.Playerid] = r
		index++
		return true
	})

	go func(m map[int64]*rewardWeek) {
		for {
			if len(m) == 0 {
				return
			}

			t := time.Now()

			for k, v := range m {
				if t.Unix() >= v.t.Unix() {
					info := arena.gm.GetPlayerInfoByID(k)
					if info == nil {
						delete(m, k)
						continue
					}

					world := arena.wm.GetWorldByID(info.ServerId)
					if world == nil {
						delete(m, k)
						continue
					}

					msg := &world_message.MUW_ArenaWeeklyReward{
						PlayerId: k,
						Score:    v.s,
					}

					world.SendProtoMessage(msg)
					delete(m, k)
				}
			}

			d := time.Since(t)
			time.Sleep(time.Millisecond - d)
		}
	}(mapReward)
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
		ArenaSeasonEndTime: arena.SeasonEndTime(),
		ArenaSeason:        arena.Season(),
		TimeStamp:          int(time.Now().Unix()),
	})

	// broadcast to all world
	msg := &world_message.MUW_SyncArenaSeason{
		Season:  int32(arena.Season()),
		EndTime: uint32(arena.SeasonEndTime()),
	}
	arena.wm.BroadCast(msg)
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

		if oldSec != newSec {
			arena.arrMatchPool[oldSec].Delete(value.Playerid)
			arena.arrMatchPool[newSec].Store(value.Playerid, struct{}{})
		}
		return true
	})

	arena.arrRankArena.Sort()
}

// save top 3 champion id
func (arena *Arena) SaveChampion() {
	list := arena.arrRankArena.GetTop(3)

	arena.cpLock.Lock()
	arena.championList = nil

	for k, v := range list {
		data := &championData{
			rank:     k,
			playerID: v.Playerid,
			score:    int(v.Score),
			season:   arena.Season(),
		}
		arena.championList = append(arena.championList, data)
	}
	arena.cpLock.Unlock()

	// save to db
	arena.ds.DB().Delete(championData{})
	arena.ds.DB().Save(arena.championList)

	// broadcast to all world
	msg := &world_message.MUW_ArenaChampion{
		Data: arena.GetChampion(),
	}

	arena.wm.BroadCast(msg)
}

// send top 100 reward mail
func (arena *Arena) seasonReward() {
	list := arena.arrRankArena.GetTop(100)

	for n, data := range list {
		info := arena.gm.GetPlayerInfoByID(data.Playerid)
		if info == nil {
			logger.Warning("arena season end, but cannot find top", n+1, " player ", data.Playerid)
			continue
		}

		world := arena.wm.GetWorldByID(info.ServerId)
		if world == nil {
			logger.Warning("arena season end, but cannot find top", n+1, " player ", data.Playerid, " world ", info.ServerId)
			continue
		}

		msg := &world_message.MUW_ArenaSeasonReward{
			PlayerId: data.Playerid,
			Rank:     int32(n + 1),
		}

		world.SendProtoMessage(msg)
	}
}

func (arena *Arena) seasonEnd() {
	arena.SaveChampion()
	arena.seasonReward()
	arena.nextSeason()
	arena.newSeasonRank()
}

// Matching begin matching
func (arena *Arena) Matching(playerID int64) {
	_, ok := arena.mapRecord.Load(playerID)

	if ok {
		// add to match request
		arena.chMatchWaitOK <- playerID

		// request newlest record after 5 seconds
		arena.mapRecordReq.Store(playerID, time.Now().Add(time.Second*5))

	} else {
		// request record
		arena.mapRecordReq.Store(playerID, time.Now())

		// add to matching wait list
		arena.matchingList.Store(playerID, struct{}{})
	}
}

// AddRecord if existing then replace record
func (arena *Arena) AddRecord(rec *world_message.ArenaRecord) {

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

// BattleResult battle end
func (arena *Arena) BattleResult(attack int64, target int64, win bool) {
	d, ok := arena.mapArenaData.Load(attack)
	if !ok {
		logger.Warning("cannot find attack ", attack, " 's arena data!")
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

// RequestRank request rank by world, max page is 10
func (arena *Arena) RequestRank(id int64, page int32) {
	if page >= 10 || page < 0 {
		logger.Warning("player ", id, " request rank error: page ", page)
		return
	}

	info := arena.gm.GetPlayerInfoByID(id)
	if info == nil {
		logger.Warning("player ", id, " request rank error: cannot find player info")
		return
	}

	world := arena.wm.GetWorldByID(info.ServerId)
	if world == nil {
		logger.Warning("player ", id, " request rank error: cannot find world ", info.ServerId)
		return
	}

	msg := &world_message.MUW_RequestArenaRank{
		PlayerId:      id,
		Page:          page,
		Score:         int32(arenaDefaultScore),
		Rank:          -1,
		SeasonEndTime: uint32(arena.SeasonEndTime()),
		Infos:         make([]*world_message.ArenaTargetInfo, 0),
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

		value := v.(*world_message.ArenaRecord)

		info := &world_message.ArenaTargetInfo{
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
		logger.Warning("reply arena request rank pages error:", msg.Page)
	}

	world.SendProtoMessage(msg)
}
