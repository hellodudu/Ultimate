package ultimate

import (
	"context"
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

// rankRecord sort interface
type rankRecord struct {
	item   []*world_message.ArenaRecord
	rwLock sync.RWMutex
}

func (s *rankRecord) Sort() {
	s.rwLock.Lock()
	defer s.rwLock.Unlock()
	sort.Sort(s)
}

func (s rankRecord) Len() int {
	return len(s.item)
}

func (s rankRecord) Length() int {
	s.rwLock.RLock()
	defer s.rwLock.RUnlock()
	return len(s.item)
}

func (s *rankRecord) Swap(a, b int) {
	s.item[a], s.item[b] = s.item[b], s.item[a]
}

func (s rankRecord) Less(a, b int) bool {
	return s.item[a].ArenaScore > s.item[b].ArenaScore
}

func (s rankRecord) Get(n int) *world_message.ArenaRecord {
	s.rwLock.RLock()
	defer s.rwLock.RUnlock()
	return s.item[n]
}

func (s *rankRecord) Add(v *world_message.ArenaRecord) {
	s.rwLock.Lock()
	defer s.rwLock.Unlock()
	s.item = append(s.item, v)
}

// Arean data
type Arena struct {
	mapRecord  sync.Map   // all player's arena record
	sMatchPool []sync.Map // 8 level match pool map[int64]struct{}
	mapRecReq  sync.Map   // map of request player arena record map[int64]struct{}
	sRankRec   rankRecord // slice of arena record sorted with ArenaScore

	chMatchWait chan int64 // match wait player channel
	endTime     int32      `sql:"arena_end_time"`

	ctx      context.Context
	cancel   context.CancelFunc
	chDBInit chan struct{}
}

func NewArena(ctx context.Context) (*Arena, error) {
	arena := &Arena{
		sRankRec:    rankRecord{item: make([]*world_message.ArenaRecord, 0)},
		sMatchPool:  make([]sync.Map, ArenaMatchSectionNum),
		chMatchWait: make(chan int64, 1000),
		chDBInit:    make(chan struct{}, 1),
	}

	arena.ctx, arena.cancel = context.WithCancel(ctx)

	return arena, nil
}

func GetSectionIndexByScore(score int32) int32 {
	if score <= 1000 {
		return 0
	} else if score <= 1100 {
		return 1
	} else if score <= 1200 {
		return 2
	} else if score <= 1300 {
		return 3
	} else if score <= 1400 {
		return 4
	} else if score <= 1500 {
		return 5
	} else if score <= 1600 {
		return 6
	} else {
		return 7
	}
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
		case id := <-arena.chMatchWait:
			logger.Info("player:", id, " start arena matching!")
			arena.UpdateMatching(id)

		default:
			t := time.Now()
			arena.UpdateRecordRequest()
			d := time.Since(t)
			time.Sleep(time.Millisecond - d)

		}
	}
}

func (arena *Arena) LoadFromDB() {
	f, ok := reflect.TypeOf(*arena).FieldByName("endTime")
	if !ok {
		logger.Warning("cannot find arena's endTime field!")
		return
	}

	query := fmt.Sprintf("select %s from global where id = %d", f.Tag.Get("sql"), global.UltimateID)

	rows, err := Instance().dbMgr.Query(query)
	if err != nil {
		logger.Warning("cannot load arena's endTime from dbMgr!")
		return
	}

	for rows.Next() {
		if err := rows.Scan(&arena.endTime); err != nil {
			logger.Warning("arena load from db failed:", err)
		}
		logger.Info("arena load from db success:", arena.endTime)
	}

	// if arena endtime was expired, set a new endtime one month later
	if int32(time.Now().Unix()) > arena.endTime {
		arena.endTime = int32(time.Now().Add(time.Hour * 24 * 30).Unix())
		query := fmt.Sprintf("update global set arena_end_time = %d", arena.endTime)
		Instance().dbMgr.Exec(query)
	}

	arena.chDBInit <- struct{}{}
}

func (arena *Arena) UpdateMatching(id int64) {

	// get player arena section
	sv, ok := arena.mapRecord.Load(id)
	if !ok {
		logger.Warning("cannot find player:", id, " 's arena record yet!")
		return
	}

	srcRec, ok := sv.(*world_message.ArenaRecord)
	if !ok {
		logger.Warning("cannot assert to arena record!")
		return
	}

	secIdx := GetSectionIndexByScore(srcRec.ArenaScore)
	arena.sMatchPool[secIdx].Range(func(k, _ interface{}) bool {
		key, ok := k.(int64)
		if !ok {
			return true
		}

		if key == id {
			return true
		}

		// todo find in next section

		dv, ok := arena.mapRecord.Load(key)
		if !ok {
			return true
		}

		dstRec, ok := dv.(*world_message.ArenaRecord)
		if !ok {
			logger.Warning("cannot assert to arena record!")
			return false
		}

		info := Instance().GetGameMgr().GetPlayerInfoByID(id)
		if info == nil {
			logger.Warning("cannot find player ", id, " s info!")
			return false
		}

		if world := Instance().GetWorldSession().GetWorldByID(info.ServerId); world != nil {
			msg := &world_message.MUW_ArenaStartBattle{
				AttackId:     id,
				TargetRecord: dstRec,
			}
			world.SendProtoMessage(msg)
		}
		return false
	})

}

func (arena *Arena) UpdateRecordRequest() {
	var arrDel []int64
	arena.mapRecReq.Range(func(k, _ interface{}) bool {
		key, ok := k.(int64)
		if !ok {
			return true
		}

		if _, ok := arena.mapRecord.Load(key); ok {
			arena.chMatchWait <- key
			arrDel = append(arrDel, key)
		}
		return true
	})

	for _, v := range arrDel {
		arena.mapRecReq.Delete(v)
	}
}

func (arena *Arena) Matching(w *World, playerID int64) {
	_, ok := arena.mapRecord.Load(playerID)

	if ok {
		// add to match request
		arena.chMatchWait <- playerID

	} else {
		// request player record
		msg := &world_message.MUW_ArenaAddRecord{
			PlayerId: playerID,
		}

		w.SendProtoMessage(msg)

		// add to record request list
		arena.mapRecReq.Store(playerID, struct{}{})
	}
}

func (arena *Arena) AddRecord(rec *world_message.ArenaRecord) {

	if _, ok := arena.mapRecord.Load(rec.PlayerId); ok {
		return
	}

	// add to record
	arena.mapRecord.Store(rec.PlayerId, rec)

	// add to slice record sorted by ArenaRecord
	arena.sRankRec.Add(rec)
	arena.sRankRec.Sort()

	// add to matching cache
	index := GetSectionIndexByScore(rec.ArenaScore)
	arena.sMatchPool[index].Store(rec.PlayerId, struct{}{})
}

func (arena *Arena) ReorderRecord(rec *world_message.ArenaRecord, preSection, newSection int32) {
	arena.sMatchPool[preSection].Delete(rec.PlayerId)
	arena.sMatchPool[newSection].Store(rec.PlayerId, struct{}{})

}

func (arena *Arena) BattleResult(atkID int64, tarID int64, win bool) {

	logger.Info("arena battle result:", atkID, tarID, win)

	v, ok := arena.mapRecord.Load(atkID)
	if !ok {
		logger.Warning("arena battle result return without record:", atkID, tarID, win)
		return
	}

	atkRec, ok := v.(*world_message.ArenaRecord)
	if !ok {
		logger.Warning("cannot assert to ArenaRecord!")
		return
	}

	if win {
		// section change
		preSection := GetSectionIndexByScore(atkRec.ArenaScore)
		atkRec.ArenaScore += 10
		newSection := GetSectionIndexByScore(atkRec.ArenaScore)
		if preSection != newSection {
			arena.ReorderRecord(atkRec, preSection, newSection)
		}

		// rank change
		arena.sRankRec.Sort()
		logger.Trace("after sort rank rec :")
		for n := 0; n < arena.sRankRec.Length(); n++ {
			t := arena.sRankRec.Get(n)
			logger.Trace("player id = ", t.PlayerId, ", player name = ", t.FirstGroup.Name, ", arena score = ", t.ArenaScore)
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
		if n >= arena.sRankRec.Length() {
			break
		}

		r := arena.sRankRec.Get(n)
		info := &world_message.ArenaTargetInfo{
			PlayerId:     r.PlayerId,
			PlayerName:   r.FirstGroup.Name,
			ServerName:   r.FirstGroup.WorldName,
			Level:        r.FirstGroup.Level,
			PlayerScore:  r.FirstGroup.PlayerScore,
			HeadProtrait: int32(r.FirstGroup.Protrait),
			HeadQuality:  int32(r.FirstGroup.HeadQuality),
			ArenaScore:   r.ArenaScore,
		}
		msg.Infos = append(msg.Infos, info)
	}

	world.SendProtoMessage(msg)
}
