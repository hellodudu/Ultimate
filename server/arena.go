package ultimate

import (
	"context"
	"fmt"
	"sort"
	"sync"
	"time"

	"github.com/hellodudu/Ultimate/global"
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
	if s.item[a].score == s.item[b].score {
		if s.item[a].reachTime == s.item[b].reachTime {
			return s.item[a].playerid < s.item[b].playerid
		}
		return s.item[a].reachTime < s.item[b].reachTime
	}
	return s.item[a].score > s.item[b].score
}

func (s *rankArenaData) Get(n int) *arenaData {
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
	playerid   int64  `sql:"player_id"`
	score      int32  `sql:"score"`
	reachTime  uint32 `sql:"reach_time"`
	lastTarget int64  `sql:"last_target"` // target cannot be last one
}

// Arena data
type Arena struct {
	mapArenaData sync.Map      // all player's arena data
	arrRankArena rankArenaData // slice of arena record sorted with ArenaScore

	mapRecord    sync.Map   // all player's arena record
	arrMatchPool []sync.Map // 8 level match pool map[playerid]struct{}
	matchingList sync.Map   // list of matching waiting player map[playerid]struct{}

	mapRecordReq sync.Map // map of init player request map[playerid]time.Now() : next request time

	chMatchWaitOK    chan int64 // match wait player channel
	season           int        `sql:"arena_season"`
	seasonEndTime    uint32     `sql:"arena_season_end_time"`
	reqNewRecordTime uint32     // every monday request new player's record

	ctx      context.Context
	cancel   context.CancelFunc
	chDBInit chan struct{}
}

// NewArena create new arena
func NewArena(ctx context.Context) (*Arena, error) {
	arena := &Arena{
		arrRankArena:     rankArenaData{item: make([]*arenaData, 0)},
		arrMatchPool:     make([]sync.Map, arenaMatchSectionNum),
		chMatchWaitOK:    make(chan int64, 1000),
		chDBInit:         make(chan struct{}, 1),
		reqNewRecordTime: 0,
	}

	arena.ctx, arena.cancel = context.WithCancel(ctx)

	return arena, nil
}

func getSectionIndexByScore(score int32) int32 {
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

func getDefaultScoreBySection(secIdx int32) int32 {
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

// GetSeasonEndTime get season end time
func (arena *Arena) GetSeasonEndTime() uint32 {
	return arena.seasonEndTime
}

// GetSeason get season
func (arena *Arena) GetSeason() int {
	return arena.season
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
			logger.Info("player:", id, " start arena matching!")

			ok, err := arena.updateMatching(id)
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
			arena.updateRequestRecord()
			arena.updateMatchingList()
			arena.updateTime()
			d := time.Since(t)
			time.Sleep(time.Millisecond - d)

		}
	}
}

// LoadFromDB load arena data from db
func (arena *Arena) LoadFromDB() {
	// seasonEndTime, ok := reflect.TypeOf(*arena).FieldByName("seasonEndTime")
	// if !ok {
	// 	logger.Warning("cannot find arena's seasonEndTime field!")
	// 	return
	// }

	// season, ok := reflect.TypeOf(*arena).FieldByName("season")
	// if !ok {
	// 	logger.Warning("cannot find arena's season field!")
	// 	return
	// }

	// load from global
	query := fmt.Sprintf("select arena_season, arena_request_new_record_time, arena_season_end_time from global where id = %d", global.UltimateID)

	rows, err := Instance().GetDBMgr().Query(query)
	if err != nil {
		logger.Error("cannot load mysql table global!")
		return
	}

	for rows.Next() {
		if err := rows.Scan(&arena.seasonEndTime, &arena.reqNewRecordTime, &arena.season); err != nil {
			logger.Error("load table global failed:", err)
		}
		logger.Info("load from global success end_time = ", arena.seasonEndTime, ", season = ", arena.season, ", reqNewRecordTime = ", arena.reqNewRecordTime)
	}

	// load from arena_player
	query = fmt.Sprintf("select * from arena_player")
	rows, err = Instance().GetDBMgr().Query(query)
	if err != nil {
		logger.Error("cannot load mysql table arena!", err)
		return
	}

	for rows.Next() {
		data := &arenaData{}
		if err := rows.Scan(&data.playerid, &data.score, &data.reachTime, &data.lastTarget); err != nil {
			logger.Warning("load table arena failed:", err)
			continue
		}
		logger.Info("load from arena success:", data)

		arena.mapArenaData.Store(data.playerid, data)

		// add to record request list, delay 20s to start request
		arena.mapRecordReq.Store(data.playerid, time.Now().Add(time.Second*20))
	}

	// if arena seasonEndTime was expired, set a new seasonEndTime one season later
	now := uint32(time.Now().Unix())
	if arena.seasonEndTime == 0 {
		arena.nextSeason()

		// if season end time has gone past, call seasonEnd() 10 minutes later(wait for all world connected)
	} else if now > arena.seasonEndTime {
		t := time.NewTimer(10 * time.Minute)
		go func() {
			<-t.C
			arena.seasonEnd()
		}()
	}

	// if arena reqNewRecordTime was expired, set a new time one week later
	if arena.reqNewRecordTime == 0 {
		arena.requestNewPlayerRecord()

		// if reqNewRecordTime has gone past, call requestNewPlayerRecord() 10 minutes later(wait for all world connected)
	} else if now > arena.reqNewRecordTime {
		t := time.NewTimer(10 * time.Minute)
		go func() {
			<-t.C
			arena.requestNewPlayerRecord()
		}()
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
			if key == data.lastTarget {
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
	secIdx := getSectionIndexByScore(data.score)
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

func (arena *Arena) updateRequestRecord() {
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

func (arena *Arena) updateMatchingList() {
	var arrDel []int64
	arena.matchingList.Range(func(k, _ interface{}) bool {
		key := k.(int64)

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

func (arena *Arena) updateTime() {
	t := uint32(time.Now().Unix())
	if t >= arena.seasonEndTime {
		arena.seasonEnd()
	}

	// todo request new player record
	if t >= arena.reqNewRecordTime {
		arena.requestNewPlayerRecord()
	}
}

func (arena *Arena) requestNewPlayerRecord() {
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

	// add 30 seconds inaccuracy
	arena.reqNewRecordTime = uint32(ct.Add(d - e + time.Duration(time.Second)*30).Unix())

	// save db
	query := fmt.Sprintf("update global set arena_request_new_record_time = %d where id = %d", int32(arena.reqNewRecordTime), global.UltimateID)
	Instance().GetDBMgr().Exec(query)

	// send request with time delay, 50 request per second
	var arrReq []int64
	arena.mapRecord.Range(func(k, _ interface{}) bool {
		id := k.(int64)
		arrReq = append(arrReq, id)
		return true
	})

	for k, v := range arrReq {
		info := Instance().GetGameMgr().GetPlayerInfoByID(v)
		if info == nil {
			continue
		}

		world := Instance().GetWorldSession().GetWorldByID(info.ServerId)
		if world == nil {
			continue
		}

		arena.mapRecordReq.Store(v, ct.Add(time.Second*time.Duration(k/50)))
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
	arena.seasonEndTime = uint32(ct.Add(d - e + time.Duration(time.Second)*30).Unix())
	arena.season++

	// save db
	query := fmt.Sprintf("update global set arena_season_end_time = %d, arena_season = %d where id = %d", int32(arena.seasonEndTime), arena.season, global.UltimateID)
	Instance().GetDBMgr().Exec(query)

	// broadcast to all world
	msg := &world_message.MUW_SyncArenaSeason{
		Season:  int32(arena.season),
		EndTime: uint32(arena.seasonEndTime),
	}
	Instance().GetWorldSession().BroadCast(msg)
}

func (arena *Arena) newSeasonRank() {
	// set all player's score to default
	arena.mapArenaData.Range(func(k, v interface{}) bool {
		value := v.(*arenaData)
		def := getDefaultScoreBySection(getSectionIndexByScore(value.score))
		value.score = def
		return true
	})

	arena.arrRankArena.Sort()
}

// send top 100 reward mail
func (arena *Arena) seasonReward() {
	for n := 0; n < arena.arrRankArena.Length(); n++ {
		if n >= 100 {
			break
		}

		data := arena.arrRankArena.Get(n)
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

func (arena *Arena) seasonEnd() {
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
			playerid:   rec.PlayerId,
			score:      int32(arenaDefaultScore),
			reachTime:  uint32(time.Now().Unix()),
			lastTarget: -1,
		}

		// use exist arena data first
		if s, ok := arena.mapArenaData.Load(rec.PlayerId); ok {
			d := s.(*arenaData)
			data.score = d.score
			data.reachTime = d.reachTime
		}

		arena.mapArenaData.Store(rec.PlayerId, data)
		arena.mapRecord.Store(rec.PlayerId, rec)

		// add to slice record sorted by ArenaRecord
		arena.arrRankArena.Add(data)
		arena.arrRankArena.Sort()

		// add to matching cache
		index := getSectionIndexByScore(data.score)
		arena.arrMatchPool[index].Store(rec.PlayerId, struct{}{})

		// save to db
		query := fmt.Sprintf("replace into arena_player set player_id = %d, score = %d, reachTime = %d, last_target = %d", data.playerid, data.score, data.reachTime, data.lastTarget)
		Instance().GetDBMgr().Exec(query)
	}
}

func (arena *Arena) reorderRecord(id int64, preSection, newSection int32) {
	arena.arrMatchPool[preSection].Delete(id)
	arena.arrMatchPool[newSection].Store(id, struct{}{})

}

// BattleResult battle end
func (arena *Arena) BattleResult(attack int64, target int64, win bool) {
	logger.Info("arena battle result:", attack, target, win)

	d, ok := arena.mapArenaData.Load(attack)
	if !ok {
		logger.Warning("cannot find attack ", attack, " 's arena data!")
		return
	}

	data := d.(*arenaData)

	if win {
		// section change
		preSection := getSectionIndexByScore(data.score)
		data.score += 10
		data.reachTime = uint32(time.Now().Unix())
		newSection := getSectionIndexByScore(data.score)
		if preSection != newSection {
			arena.reorderRecord(attack, preSection, newSection)
		}

		// save to db
		query := fmt.Sprintf("replace into arena set player_id = %d, score = %d, reachTime = %d, last_target = %d", data.playerid, data.score, data.reachTime, data.lastTarget)
		Instance().GetDBMgr().Exec(query)

		// rank change
		arena.arrRankArena.Sort()
		logger.Trace("after sort rank rec :")
		for n := 0; n < arena.arrRankArena.Length(); n++ {
			t := arena.arrRankArena.Get(n)
			logger.Trace("player id = ", t.playerid, ", arena score = ", t.score, ", reach time = ", t.reachTime)
		}
	}
}

// RequestRank request rank by world, max page is 10
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

	for n := 0 + int(page)*arenaRankNumPerPage; n < 10+int(page)*arenaRankNumPerPage; n++ {
		if n >= arena.arrRankArena.Length() {
			break
		}

		r := arena.arrRankArena.Get(n)
		v, ok := arena.mapRecord.Load(r.playerid)
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
			HeadProtrait: int32(value.FirstGroup.Protrait),
			HeadQuality:  int32(value.FirstGroup.HeadQuality),
			ArenaScore:   r.score,
		}
		msg.Infos = append(msg.Infos, info)
	}

	world.SendProtoMessage(msg)
}
