package arena

import (
	"context"
	"fmt"
	"strings"
	"sync"
	"time"

	"github.com/hellodudu/Ultimate/iface"
	pbArena "github.com/hellodudu/Ultimate/proto/arena"
	pbGame "github.com/hellodudu/Ultimate/proto/game"
	"github.com/micro/go-micro"
	logger "github.com/sirupsen/logrus"
)

var arenaMatchSectionNum = 8 // arena section num
var arenaRankNumPerPage = 10
var arenaSeasonDays = 4 * 7       // one season = 4 weeks
var arenaRequestNewRecordDays = 7 // every week request new player record
var arenaDefaultScore = 1000      // default arena score

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
	def := 1000
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
	return int32(def)
}

// Arena data
type Arena struct {
	ds           iface.IDatastore
	mapArenaData map[int64]*arenaData // all player's arena data
	arrRankArena rankArenaData        // slice of arena record sorted with ArenaScore

	mapRecord    sync.Map   // all player's arena record
	arrMatchPool []sync.Map // 8 level match pool map[playerid]struct{}
	matchingList sync.Map   // list of matching waiting player map[playerid]struct{}

	mapRecordReq sync.Map // map of init player request map[playerid]time.Now() : next request time

	chMatchWaitOK chan int64 // match wait player channel

	championList []*championData // top 3 champion data
	lock         sync.RWMutex    // champion read write lock

	ctx      context.Context
	cancel   context.CancelFunc
	chDBInit chan struct{}
	chStop   chan struct{}

	handler *RPCHandler
	pubsub  *pubSub
}

// NewArena create new arena
func NewArena(ctx context.Context, service micro.Service, ds iface.IDatastore) (*Arena, error) {
	arena := &Arena{
		ds:            ds,
		arrRankArena:  rankArenaData{item: make([]*arenaData, 0)},
		mapArenaData:  make(map[int64]*arenaData),
		arrMatchPool:  make([]sync.Map, arenaMatchSectionNum),
		chMatchWaitOK: make(chan int64, 1000),
		chDBInit:      make(chan struct{}, 1),
		chStop:        make(chan struct{}, 1),
		championList:  make([]*championData, 0),
	}

	arena.ctx, arena.cancel = context.WithCancel(ctx)
	arena.handler = &RPCHandler{
		ctx:   arena.ctx,
		arena: arena,
		gameSrv: pbGame.NewGameService(
			"ultimate-service-game",
			service.Client(),
		),
	}

	// register Handler
	pbArena.RegisterArenaServiceHandler(service.Server(), arena.handler)

	// create pub/sub manager
	arena.pubsub = newPubSub(service, arena)

	go arena.loadFromDB()
	go arena.run()

	return arena, nil
}

func (arena *Arena) getArenaDataNum() int {
	arena.lock.RLock()
	defer arena.lock.RUnlock()
	return len(arena.mapArenaData)
}

func (arena *Arena) GetDataByID(id int64) (interface{}, error) {
	arena.lock.RLock()
	defer arena.lock.RUnlock()

	v, ok := arena.mapArenaData[id]
	if !ok {
		return nil, fmt.Errorf("cannot find arena data with id %d", id)
	}

	return v, nil
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

func (arena *Arena) getRecordReqList() map[int64]int64 {
	ret := make(map[int64]int64)
	arena.mapRecordReq.Range(func(k, v interface{}) bool {
		ret[k.(int64)] = v.(int64)
		return true
	})
	return ret
}

func (arena *Arena) getRankListByPage(page int) []*arenaData {
	return arena.arrRankArena.getListByPage(page)
}

// seasonEndTime get season end time
func (arena *Arena) seasonEndTime() int {
	return arena.ds.TableGlobal().ArenaSeasonEndTime
}

// season get season
func (arena *Arena) season() int {
	return arena.ds.TableGlobal().ArenaSeason
}

func (arena *Arena) fixEndTime() int {
	return arena.ds.TableGlobal().ArenaFixEndTime
}

func (arena *Arena) WeekEndTime() int {
	return arena.ds.TableGlobal().ArenaWeekEndTime
}

func (arena *Arena) getChampion() []*pbArena.ArenaChampion {
	arena.lock.RLock()
	defer arena.lock.RUnlock()

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
	<-arena.chStop
	close(arena.chMatchWaitOK)
	close(arena.chDBInit)
	close(arena.chStop)
}

func (arena *Arena) run() {
	<-arena.chDBInit

	for {
		select {
		// context canceled
		case <-arena.ctx.Done():
			logger.Info("arena context done!")
			arena.chStop <- struct{}{}
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
			time.Sleep(time.Millisecond*200 - d)

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

	arena.lock.Lock()
	for k, v := range arenaDataList {
		arena.mapArenaData[v.Playerid] = v

		// add to record request list, delay 20s to start request
		arena.mapRecordReq.Store(v.Playerid, time.Now().Add(time.Second*time.Duration((20+k/30))).Unix())

		// add to slice record sorted by ArenaRecord
		arena.arrRankArena.Add(v)

	}
	arena.lock.Unlock()

	// resort
	arena.arrRankArena.Sort()

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

	// fix arena end time
	if arena.ds.TableGlobal().ArenaFixEndTime == 0 {
		arena.newSeasonRank()
		arena.ds.TableGlobal().ArenaFixEndTime = 1
		// arena.ds.TableGlobal().ArenaWeekEndTime = arena.WeekEndTime() - 490

		// save to db
		arena.ds.DB().Model(arena.ds.TableGlobal()).Updates(iface.TableGlobal{
			// ArenaWeekEndTime: arena.WeekEndTime(),
			ArenaFixEndTime: arena.fixEndTime(),
			TimeStamp:       int(time.Now().Unix()),
		})
		logger.Print("set new season rank")
	}

	// all init ok
	arena.chDBInit <- struct{}{}
}

func (arena *Arena) updateMatching(id int64) (bool, error) {

	// get player arena data
	arena.lock.RLock()
	d, ok := arena.mapArenaData[id]
	data := *d
	arena.lock.RUnlock()

	if !ok {
		logger.WithFields(logger.Fields{
			"player_id": id,
		}).Warn("cannot find player's arena data")
		return false, fmt.Errorf("cannot find player %d 's arena data", id)
	}

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
		logger.WithFields(logger.Fields{
			"player_id": id,
			"error":     err,
		}).Warn("cannot find player's info")
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

	arena.pubsub.publishSendWorldMessage(arena.ctx, resp.Info.ServerId, msg)
	return true, nil
}

func (arena *Arena) updateRequestRecord() {

	num := 0
	idList := make([]int64, 0)
	arena.mapRecordReq.Range(func(k, v interface{}) bool {
		id := k.(int64)
		t := v.(int64)

		// every tick request 30 record
		if num >= 30 {
			return false
		}

		// it's not right time to send request
		if time.Now().Unix() < t {
			return true
		}

		idList = append(idList, id)
		num++
		return true
	})

	for _, v := range idList {

		go func(id int64, ctx context.Context) {
			if resp, err := arena.handler.GetPlayerInfoByID(id); err == nil {

				// send request and try again 30 seconds later
				msg := &pbArena.MUW_ArenaAddRecord{
					PlayerId: id,
				}

				arena.pubsub.publishSendWorldMessage(ctx, resp.Info.ServerId, msg)

				// try again 30 seconds later
				arena.mapRecordReq.Store(id, time.Now().Add(time.Second*30).Unix())
			}
		}(v, arena.ctx)

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
	o := time.Duration(time.Hour) * time.Duration(24)
	d := o * 7

	// now elapse duration
	e := time.Hour*time.Duration(24)*time.Duration(cw-1) + time.Hour*time.Duration(ct.Hour()) + time.Minute*time.Duration(ct.Minute()) + time.Second*time.Duration(ct.Second())

	// add 10 seconds inaccuracy
	arena.ds.TableGlobal().ArenaWeekEndTime = int(ct.Add(d + o*7 - e - time.Duration(time.Minute)*8).Unix())

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

	for k, v := range arrReq {
		_, err := arena.handler.GetPlayerInfoByID(v)
		if err != nil {
			continue
		}

		arena.mapRecordReq.Store(v, ct.Add(time.Second*time.Duration(k/30)).Unix())

	}

	// send weekly reward
	type rewardWeek struct {
		id int64 // player_id
		s  int32 // score
	}

	// map[world_id]arrayRewardWeek
	mapWeekReward := make(map[uint32]([]*rewardWeek))
	arena.lock.RLock()
	for _, v := range arena.mapArenaData {
		r := &rewardWeek{
			id: v.Playerid,
			s:  v.Score,
		}

		resp, err := arena.handler.GetPlayerInfoByID(r.id)
		if err != nil {
			continue
		}

		if _, ok := mapWeekReward[resp.Info.ServerId]; !ok {
			mapWeekReward[resp.Info.ServerId] = make([]*rewardWeek, 0)
		}

		mapWeekReward[resp.Info.ServerId] = append(mapWeekReward[resp.Info.ServerId], r)
	}
	arena.lock.RUnlock()

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

		arena.pubsub.publishSendWorldMessage(arena.ctx, worldID, msg)
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
	o := time.Duration(time.Hour) * time.Duration(24)
	d := o * time.Duration(arenaSeasonDays)

	// now elapse duration
	e := time.Hour*time.Duration(24)*time.Duration(cw-1) + time.Hour*time.Duration(ct.Hour()) + time.Minute*time.Duration(ct.Minute()) + time.Second*time.Duration(ct.Second())

	// cut 5 minutes inaccuracy
	arena.ds.TableGlobal().ArenaSeasonEndTime = int(ct.Add(d + (o*7 - e) - time.Duration(time.Minute)*5).Unix())
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
	arena.pubsub.publishBroadCast(arena.ctx, msg)
}

func (arena *Arena) newSeasonRank() {
	// people who's section > 4, set score to 4 section default score
	// people who's section <= 4, set score to section - 1 default score

	saveList := make([]*arenaData, 0)
	arena.lock.Lock()
	for _, value := range arena.mapArenaData {
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

		saveValue := &arenaData{
			Playerid:   value.Playerid,
			Score:      value.Score,
			ReachTime:  value.ReachTime,
			LastTarget: value.LastTarget,
		}
		saveList = append(saveList, saveValue)

		if oldSec != newSec {
			arena.arrMatchPool[oldSec].Delete(value.Playerid)
			arena.arrMatchPool[newSec].Store(value.Playerid, struct{}{})
		}
	}
	arena.lock.Unlock()

	// save to db
	if len(saveList) > 0 {
		var querys []string
		querys = append(querys, "replace into arena_player values ")
		for k, v := range saveList {
			if k > 0 {
				querys = append(querys, ",")
			}

			querys = append(querys, fmt.Sprintf("(%d,%d,%d,%d)", v.Playerid, v.Score, v.ReachTime, v.LastTarget))
		}
		arena.ds.DB().Exec(strings.Join(querys, ""))
	}

	// resort
	arena.arrRankArena.Sort()
}

// save top 3 champion id
func (arena *Arena) saveChampion() {
	list := arena.arrRankArena.getTop(3)

	arena.lock.Lock()
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
	arena.lock.Unlock()

	// save to db
	arena.ds.DB().Delete(championData{})
	for _, v := range arena.championList {
		arena.ds.DB().Save(v)
	}

	// broadcast to all world
	msg := &pbArena.MUW_ArenaChampion{
		Data: arena.getChampion(),
	}

	arena.pubsub.publishBroadCast(arena.ctx, msg)
}

func (arena *Arena) SyncArenaSeason(id uint32) {
	// broadcast to all world
	msg := &pbArena.MUW_SyncArenaSeason{
		Season:  int32(arena.season()),
		EndTime: uint32(arena.seasonEndTime()),
	}
	arena.pubsub.publishBroadCast(arena.ctx, msg)
}

// send top 100 reward mail
func (arena *Arena) seasonReward() {
	list := arena.arrRankArena.getTop(100)

	for n, data := range list {
		resp, err := arena.handler.GetPlayerInfoByID(data.Playerid)
		if err != nil {
			logger.WithFields(logger.Fields{
				"top":       n + 1,
				"player_id": data.Playerid,
				"error":     err,
			}).Warn("season reward cannot find player")
			continue
		}

		msg := &pbArena.MUW_ArenaSeasonReward{
			PlayerId: data.Playerid,
			Rank:     int32(n + 1),
		}

		arena.pubsub.publishSendWorldMessage(arena.ctx, resp.Info.ServerId, msg)
	}
}

func (arena *Arena) seasonEnd() {
	arena.saveChampion()
	arena.seasonReward()
	arena.nextSeason()
	arena.newSeasonRank()
}

func (arena *Arena) matching(playerID int64) {

	_, ok := arena.mapRecord.Load(playerID)

	if ok {
		// add to match request
		arena.chMatchWaitOK <- playerID

		// request newlest record after 5 seconds
		arena.mapRecordReq.Store(playerID, time.Now().Add(time.Second*5).Unix())

	} else {
		// request record
		arena.mapRecordReq.Store(playerID, time.Now().Unix())

		// add to matching wait list
		arena.matchingList.Store(playerID, struct{}{})
	}
}

// addRecord if existing then replace record
func (arena *Arena) addRecord(rec *pbArena.ArenaRecord) {
	arena.lock.Lock()
	defer arena.lock.Unlock()

	// add new arena data
	if _, ok := arena.mapArenaData[rec.PlayerId]; !ok {

		// add new record and arena data
		data := &arenaData{
			Playerid:   rec.PlayerId,
			Score:      int32(arenaDefaultScore),
			ReachTime:  uint32(time.Now().Unix()),
			LastTarget: -1,
		}

		arena.mapArenaData[rec.PlayerId] = data

		// add to slice record sorted by ArenaRecord
		arena.arrRankArena.Add(data)
		arena.arrRankArena.Sort()

		// save to db
		arena.ds.DB().Save(data)
	}

	// add arena record
	arena.mapRecord.Store(rec.PlayerId, rec)

	// delete from request list
	arena.mapRecordReq.Delete(rec.PlayerId)

	// add to matching pool
	if v, ok := arena.mapArenaData[rec.PlayerId]; ok {
		index := getSectionIndexByScore(v.Score)
		arena.arrMatchPool[index].Store(v.Playerid, struct{}{})
	}
}

func (arena *Arena) reorderRecord(id int64, preSection, newSection int32) {
	arena.arrMatchPool[preSection].Delete(id)
	arena.arrMatchPool[newSection].Store(id, struct{}{})
}

// battleResult battle end
func (arena *Arena) battleResult(attack int64, target int64, win bool) {
	arena.lock.Lock()
	defer arena.lock.Unlock()

	data, ok := arena.mapArenaData[attack]
	if !ok {
		logger.WithFields(logger.Fields{
			"player_id": attack,
		}).Warn("cannot find attacker's arena data")
		return
	}

	// record target into last target
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

	logger.WithFields(logger.Fields{
		"attack_id":    attack,
		"target_id":    target,
		"win":          win,
		"attack_score": data.Score,
	}).Info("arena battle result")
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
	arena.lock.Lock()
	if data, ok := arena.mapArenaData[id]; ok {
		msg.Score = data.Score
		msg.Rank = int32(arena.arrRankArena.getIndexBefore100(data))
	}
	arena.lock.Unlock()

	// rank player data
	l := arena.arrRankArena.getListByPage(int(page))
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
		logger.WithFields(logger.Fields{"page": msg.Page}).Warn("reply arena request rank pages error")
	}

	arena.pubsub.publishSendWorldMessage(arena.ctx, resp.Info.ServerId, msg)
}

func (arena *Arena) APIRequestRank(id int64, page int) *pbArena.MUW_RequestArenaRank {
	if page >= 10 || page < 0 {
		logger.WithFields(logger.Fields{
			"player_id": id,
			"page":      page,
		}).Warn("api request rank error")
		return nil
	}

	msg := &pbArena.MUW_RequestArenaRank{
		PlayerId:      id,
		Page:          int32(page),
		Score:         int32(arenaDefaultScore),
		Rank:          -1,
		SeasonEndTime: uint32(arena.seasonEndTime()),
		Infos:         make([]*pbArena.ArenaTargetInfo, 0),
	}

	logger.Warning("get arena request rank page:", msg.Page)

	// get player rank
	arena.lock.Lock()
	if data, ok := arena.mapArenaData[id]; ok {
		msg.Score = data.Score
		msg.Rank = int32(arena.arrRankArena.getIndexBefore100(data))
	}
	arena.lock.Unlock()

	// rank player data
	l := arena.arrRankArena.getListByPage(page)
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
		logger.Warning("reply arena api request rank pages error:", msg.Page)
	}

	return msg
}
