package ultimate

import (
	"context"
	"errors"
	"fmt"
	"reflect"
	"sort"
	"sync"
	"time"

	"github.com/hellodudu/Ultimate/global"
	"github.com/hellodudu/Ultimate/logger"
	world_message "github.com/hellodudu/Ultimate/proto"
)

var ArenaMatchSectionNum int = 8 // arena section num
var ArenaRankNumPerPage int = 10
var ArenaSeasonDays int = 4 * 7    // one season = 4 weeks
var ArenaDefaultScore int32 = 1000 // default arena score

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

func (s rankArenaData) Len() int {
	return len(s.item)
}

func (s rankArenaData) Length() int {
	s.rwLock.RLock()
	defer s.rwLock.RUnlock()
	return len(s.item)
}

func (s *rankArenaData) Swap(a, b int) {
	s.item[a], s.item[b] = s.item[b], s.item[a]
}

func (s rankArenaData) Less(a, b int) bool {
	if s.item[a].score == s.item[b].score {
		if s.item[a].reach_time == s.item[b].reach_time {
			return s.item[a].playerid < s.item[b].playerid
		}
		return s.item[a].reach_time < s.item[b].reach_time
	}
	return s.item[a].score > s.item[b].score
}

func (s rankArenaData) Get(n int) *arenaData {
	s.rwLock.RLock()
	defer s.rwLock.RUnlock()
	return s.item[n]
}

func (s *rankArenaData) Add(v *arenaData) {
	s.rwLock.Lock()
	defer s.rwLock.Unlock()
	s.item = append(s.item, v)
}

// arena player data
type arenaData struct {
	playerid   int64 `sql:"player_id"`
	score      int32 `sql:"score"`
	reach_time int32 `sql:"reach_time"`
}

// Arean data
type Arena struct {
	mapArenaData sync.Map      // all player's arena data
	sRankArena   rankArenaData // slice of arena record sorted with ArenaScore

	mapRecord    sync.Map   // all player's arena record
	arrMatchPool []sync.Map // 8 level match pool map[playerid]struct{}
	matchingList sync.Map   // list of matching waiting player map[playerid]struct{}

	mapRecordReq sync.Map // map of init player request map[playerid]time.Now() : next request time

	chMatchWaitOK chan int64 // match wait player channel
	season        int        `sql:"arena_season"`
	endTime       uint32     `sql:"arena_end_time"`

	ctx      context.Context
	cancel   context.CancelFunc
	chDBInit chan struct{}
}

func NewArena(ctx context.Context) (*Arena, error) {
	arena := &Arena{
		sRankArena:    rankArenaData{item: make([]*arenaData, 0)},
		arrMatchPool:  make([]sync.Map, ArenaMatchSectionNum),
		chMatchWaitOK: make(chan int64, 1000),
		chDBInit:      make(chan struct{}, 1),
	}

	arena.ctx, arena.cancel = context.WithCancel(ctx)

	return arena, nil
}

func GetSectionIndexByScore(score int32) int32 {
	if score < 1200 {
		return 0
	} else if score <= 1400 {
		return 1
	} else if score <= 1600 {
		return 2
	} else if score <= 1800 {
		return 3
	} else if score <= 2000 {
		return 4
	} else if score <= 2300 {
		return 5
	} else if score <= 2600 {
		return 6
	} else {
		return 7
	}
}

func GetDefaultScoreBySection(secIdx int32) int32 {
	var def int32 = 1000
	switch secIdx {
	case 0:
		def = 1000
	case 1:
		def = 1200
	case 2:
		def = 1400
	case 3:
		def = 1600
	case 4:
		def = 1800
	case 5:
		def = 2000
	case 6:
		def = 2300
	case 7:
		def = 2600
	default:
		def = 1000
	}
	return def
}

func (arena *Arena) GetEndTime() uint32 {
	return arena.endTime
}

func (arena *Arena) GetSeason() int {
	return arena.season
}

func (arena *Arena) Stop() {
	arena.cancel()
}

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
			logger.Info("player:", id, " start arena matching!")

			ok, err := arena.UpdateMatching(id)
			if err != nil {
				continue
			}

			if !ok {
				// try again 5 seconds later
				t := time.NewTimer(5 * time.Second)
				go func(p int64) {
					<-t.C
					arena.chMatchWaitOK <- p
					logger.Trace("player ", p, " has no target try matching again in 5 seconds!")
				}(id)
			}

		default:
			t := time.Now()
			arena.UpdateRequestRecord()
			arena.UpdateMatchingList()
			arena.UpdateSeasonEnd()
			d := time.Since(t)
			time.Sleep(time.Millisecond - d)

		}
	}
}

func (arena *Arena) LoadFromDB() {
	endtime, ok := reflect.TypeOf(*arena).FieldByName("endTime")
	if !ok {
		logger.Warning("cannot find arena's endTime field!")
		return
	}

	season, ok := reflect.TypeOf(*arena).FieldByName("season")
	if !ok {
		logger.Warning("cannot find arena's season field!")
		return
	}

	// load from global
	query := fmt.Sprintf("select %s, %s from global where id = %d", endtime.Tag.Get("sql"), season.Tag.Get("sql"), global.UltimateID)

	rows, err := Instance().GetDBMgr().Query(query)
	if err != nil {
		logger.Warning("cannot load mysql table global!")
		return
	}

	for rows.Next() {
		if err := rows.Scan(&arena.endTime, &arena.season); err != nil {
			logger.Warning("load table global failed:", err)
		}
		logger.Info("load from global success end_time = ", arena.endTime, ", season = ", arena.season)
	}

	query = fmt.Sprintf("select * from arena")
	rows, err = Instance().GetDBMgr().Query(query)
	if err != nil {
		logger.Warning("cannot load mysql table arena!", err)
		return
	}

	for rows.Next() {
		data := &arenaData{}
		if err := rows.Scan(&data.playerid, &data.score, &data.reach_time); err != nil {
			logger.Warning("load table arena failed:", err)
		}
		logger.Info("load from arena success:", data)

		arena.mapArenaData.Store(data.playerid, data)

		// add to record request list, delay 20s to start request
		arena.mapRecordReq.Store(data.playerid, time.Now().Add(time.Second*20))
	}

	// if arena endtime was expired, set a new endtime one season later
	if arena.endTime == 0 {
		arena.NextSeason()

		// if season end time has gone past, call SeasonEnd() 10 minutes later(wait for all world connected)
	} else if uint32(time.Now().Unix()) > arena.endTime {
		t := time.NewTimer(1 * time.Minute)
		go func() {
			<-t.C
			arena.SeasonEnd()
		}()
	}

	arena.chDBInit <- struct{}{}
}

func (arena *Arena) UpdateMatching(id int64) (bool, error) {

	// get player arena data
	d, ok := arena.mapArenaData.Load(id)
	if !ok {
		logger.Warning("cannot find player:", id, " 's arena data!")
		return false, errors.New(fmt.Sprintf("cannot find player %d 's arena data!", id))
	}

	data, ok := d.(*arenaData)
	if !ok {
		logger.Warning("cannot assert player's arenadata!")
		return false, errors.New(fmt.Sprintf("cannot assert player %d 's arena data!", id))
	}

	// function of find target
	f := func(sec int32) *world_message.ArenaRecord {
		var r *world_message.ArenaRecord = nil
		arena.arrMatchPool[sec].Range(func(k, _ interface{}) bool {
			key, ok := k.(int64)
			if !ok {
				return true
			}

			if key == id {
				return true
			}

			dv, ok := arena.mapRecord.Load(key)
			if !ok {
				return true
			}

			r, ok = dv.(*world_message.ArenaRecord)
			if !ok {
				logger.Warning("cannot assert to arena record!")
				return false
			}

			return false
		})
		return r
	}

	// find in same section
	secIdx := GetSectionIndexByScore(data.score)
	dstRec := f(secIdx)

	// find in below section
	if dstRec == nil {
		for n := secIdx - 1; n >= 0; n-- {
			dstRec = f(secIdx)
			if dstRec != nil {
				break
			}
		}
	}

	// still can not find target
	if dstRec == nil {
		return false, nil
	}

	info := Instance().GetGameMgr().GetPlayerInfoByID(id)
	if info == nil {
		logger.Warning("cannot find player ", id, " s info!")
		return false, nil
	}

	if world := Instance().GetWorldSession().GetWorldByID(info.ServerId); world != nil {
		msg := &world_message.MUW_ArenaStartBattle{
			AttackId:     id,
			TargetRecord: dstRec,
		}
		world.SendProtoMessage(msg)
	}

	return true, nil
}

func (arena *Arena) UpdateRequestRecord() {
	var arrDel []int64
	var arrDelay []int64

	arena.mapRecordReq.Range(func(k, v interface{}) bool {
		id := k.(int64)
		t := v.(time.Time)

		// already has a record
		if _, ok := arena.mapRecord.Load(id); ok {
			arrDel = append(arrDel, id)
			return true
		}

		// it's not right time to send request
		if time.Now().Unix() < t.Unix() {
			return true
		}

		info := Instance().GetGameMgr().GetPlayerInfoByID(id)
		if info == nil {
			return true
		}

		world := Instance().GetWorldSession().GetWorldByID(info.ServerId)
		if world == nil {
			return true
		}

		// send request and try again 1 minutes later
		msg := &world_message.MUW_ArenaAddRecord{
			PlayerId: id,
		}
		world.SendProtoMessage(msg)
		arrDelay = append(arrDelay, id)
		return true
	})

	for _, v := range arrDel {
		arena.mapRecordReq.Delete(v)
	}

	for _, v := range arrDelay {
		arena.mapRecordReq.Store(v, time.Now().Add(time.Minute))
	}
}

func (arena *Arena) UpdateMatchingList() {
	var arrDel []int64
	arena.matchingList.Range(func(k, _ interface{}) bool {
		key, ok := k.(int64)
		if !ok {
			return true
		}

		if _, ok := arena.mapRecord.Load(key); ok {
			arena.chMatchWaitOK <- key
			arrDel = append(arrDel, key)
		}
		return true
	})

	for _, v := range arrDel {
		arena.matchingList.Delete(v)
	}
}

func (arena *Arena) UpdateSeasonEnd() {
	if uint32(time.Now().Unix()) >= arena.endTime {
		arena.SeasonEnd()
	}
}

func (arena *Arena) NextSeason() {
	// current time
	ct := time.Now()

	// current weekday
	cw := time.Now().Weekday()
	if cw == 0 {
		cw = 7
	}

	// one full season duration
	d := time.Duration(time.Hour) * time.Duration(24) * time.Duration(ArenaSeasonDays)

	// now elapse duration
	e := time.Hour*time.Duration(24)*time.Duration(cw-1) + time.Hour*time.Duration(ct.Hour()) + time.Minute*time.Duration(ct.Minute()) + time.Second*time.Duration(ct.Second())

	// add 30 seconds inaccuracy
	arena.endTime = uint32(time.Now().Add(d - e + time.Duration(time.Second)*30).Unix())
	arena.season++

	// save db
	query := fmt.Sprintf("update global set arena_end_time = %d, arena_season = %d", int32(arena.endTime), arena.season)
	Instance().GetDBMgr().Exec(query)

	// broadcast to all world
	msg := &world_message.MUW_SyncArenaSeason{
		Season:  int32(arena.season),
		EndTime: uint32(arena.endTime),
	}
	Instance().GetWorldSession().BroadCast(msg)
}

func (arena *Arena) SaveSeasonResult() {
	// save top 100 rank data
	rankLen := arena.sRankArena.Length()
	if rankLen > 100 {
		rankLen = 100
	}

	for n := 0; n < rankLen; n++ {
		if data := arena.sRankArena.Get(n); data != nil {
			query := fmt.Sprintf("replace into arena set rank=%d, season_end_time=%d, player_id=%d", n, arena.endTime, data.playerid)
			Instance().GetDBMgr().Exec(query)
		}
	}
}

func (arena *Arena) NewSeasonRank() {
	// todo clear rank
}

// send top 100 reward mail
func (arena *Arena) SeasonReward() {
	for n := 0; n < arena.sRankArena.Length(); n++ {
		if n >= 100 {
			break
		}

		data := arena.sRankArena.Get(n)
		info := Instance().GetGameMgr().GetPlayerInfoByID(data.playerid)
		if info == nil {
			logger.Warning("arena season end, but cannot find top", n+1, " player ", data.playerid)
			continue
		}

		world := Instance().GetWorldSession().GetWorldByID(info.ServerId)
		if world == nil {
			logger.Warning("arena season end, but cannot find top", n+1, " player ", data.playerid, " world ", info.ServerId)
			continue
		}

		msg := &world_message.MUW_ArenaSeasonReward{
			PlayerId: data.playerid,
			Rank:     int32(n + 1),
		}

		world.SendProtoMessage(msg)
	}
}

func (arena *Arena) SeasonEnd() {
	arena.SaveSeasonResult()
	arena.SeasonReward()
	arena.NextSeason()
	arena.NewSeasonRank()
}

func (arena *Arena) Matching(playerID int64) {
	_, ok := arena.mapRecord.Load(playerID)

	if ok {
		// add to match request
		arena.chMatchWaitOK <- playerID

	} else {
		// request record
		arena.mapRecordReq.Store(playerID, time.Now())

		// add to matching wait list
		arena.matchingList.Store(playerID, struct{}{})
	}
}

// if existing then replace record
func (arena *Arena) AddRecord(rec *world_message.ArenaRecord) {

	if _, ok := arena.mapRecord.Load(rec.PlayerId); ok {
		// update record
		arena.mapRecord.Store(rec.PlayerId, rec)
	} else {
		// add new record and arena data
		data := &arenaData{
			playerid:   rec.PlayerId,
			score:      ArenaDefaultScore,
			reach_time: int32(time.Now().Unix()),
		}

		// use exist arena data first
		if s, ok := arena.mapArenaData.Load(rec.PlayerId); ok {
			if d, ok := s.(*arenaData); ok {
				data.score = d.score
				data.reach_time = d.reach_time
			}
		}

		arena.mapArenaData.Store(rec.PlayerId, data)
		arena.mapRecord.Store(rec.PlayerId, rec)

		// add to slice record sorted by ArenaRecord
		arena.sRankArena.Add(data)
		arena.sRankArena.Sort()

		// add to matching cache
		index := GetSectionIndexByScore(data.score)
		arena.arrMatchPool[index].Store(rec.PlayerId, struct{}{})

		// save to db
		query := fmt.Sprintf("replace into arena set player_id = %d, score = %d, reach_time = %d", data.playerid, data.score, data.reach_time)
		Instance().GetDBMgr().Exec(query)
	}
}

func (arena *Arena) ReorderRecord(id int64, preSection, newSection int32) {
	arena.arrMatchPool[preSection].Delete(id)
	arena.arrMatchPool[newSection].Store(id, struct{}{})

}

func (arena *Arena) BattleResult(attack int64, target int64, win bool) {
	logger.Info("arena battle result:", attack, target, win)

	d, ok := arena.mapArenaData.Load(attack)
	if !ok {
		logger.Warning("cannot find attack ", attack, " 's arena data!")
		return
	}

	data, ok := d.(*arenaData)
	if !ok {
		logger.Warning("cannot assert to arenaData!")
		return
	}

	if win {
		// section change
		preSection := GetSectionIndexByScore(data.score)
		data.score += 10
		data.reach_time = int32(time.Now().Unix())
		newSection := GetSectionIndexByScore(data.score)
		if preSection != newSection {
			arena.ReorderRecord(attack, preSection, newSection)
		}

		// save to db
		query := fmt.Sprintf("replace into arena set player_id = %d, score = %d, reach_time = %d", data.playerid, data.score, data.reach_time)
		Instance().GetDBMgr().Exec(query)

		// rank change
		arena.sRankArena.Sort()
		logger.Trace("after sort rank rec :")
		for n := 0; n < arena.sRankArena.Length(); n++ {
			t := arena.sRankArena.Get(n)
			logger.Trace("player id = ", t.playerid, ", arena score = ", t.score, ", reach time = ", t.reach_time)
		}
	}
}

func (arena *Arena) RequestRank(id int64, page int32) {
	if page >= 10 {
		return
	}

	info := Instance().GetGameMgr().GetPlayerInfoByID(id)
	if info == nil {
		return
	}

	world := Instance().GetWorldSession().GetWorldByID(info.ServerId)
	if world == nil {
		return
	}

	msg := &world_message.MUW_RequestArenaRank{
		PlayerId: id,
		Page:     page,
		Infos:    make([]*world_message.ArenaTargetInfo, 0),
	}

	for n := 0 + int(page)*ArenaRankNumPerPage; n < 10+int(page)*ArenaRankNumPerPage; n++ {
		if n >= arena.sRankArena.Length() {
			break
		}

		r := arena.sRankArena.Get(n)
		v, ok := arena.mapRecord.Load(r.playerid)
		if !ok {
			continue
		}

		value, ok := v.(*world_message.ArenaRecord)
		if !ok {
			continue
		}

		info := &world_message.ArenaTargetInfo{
			PlayerId:     value.PlayerId,
			PlayerName:   value.FirstGroup.Name,
			ServerName:   value.FirstGroup.WorldName,
			Level:        value.FirstGroup.Level,
			PlayerScore:  value.FirstGroup.PlayerScore,
			HeadProtrait: int32(value.FirstGroup.Protrait),
			HeadQuality:  int32(value.FirstGroup.HeadQuality),
			ArenaScore:   r.score,
		}
		msg.Infos = append(msg.Infos, info)
	}

	world.SendProtoMessage(msg)
}
