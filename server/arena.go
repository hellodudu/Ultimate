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

var ArenaMatchSectionNum int32 = 8 // arena section num

// ArenaRecord sort interface
type SliceArenaRecord []*world_message.ArenaRecord

func (s SliceArenaRecord) Len() int {
	return len(s)
}

func (s SliceArenaRecord) Swap(a, b int) {
	s[a], s[b] = s[b], s[a]
}

func (s SliceArenaRecord) Less(a, b int) bool {
	return s[a].ArenaScore > s[b].ArenaScore
}

// Arean data
type Arena struct {
	mapRecord     map[int64]*world_message.ArenaRecord // all player's arena record
	sliceRecord   SliceArenaRecord                     // slice of arena record sorted with ArenaScore
	listMatchPool []map[int64]struct{}                 // 8 level match pool
	listRecReq    map[int64]struct{}                   // list to request player arena record
	chMatchWait   chan int64                           // match wait player channel
	endTime       int32                                `sql:"arena_end_time"`

	ctx      context.Context
	cancel   context.CancelFunc
	mu       sync.Mutex
	chDBInit chan struct{}
}

func NewArena(ctx context.Context) (*Arena, error) {
	arena := &Arena{
		mapRecord:     make(map[int64]*world_message.ArenaRecord),
		sliceRecord:   make(SliceArenaRecord, 0),
		listMatchPool: make([]map[int64]struct{}, ArenaMatchSectionNum),
		listRecReq:    make(map[int64]struct{}, 1000),
		chMatchWait:   make(chan int64, 1000),
		chDBInit:      make(chan struct{}, 1),
	}

	for n := 0; int32(n) < ArenaMatchSectionNum; n++ {
		arena.listMatchPool[n] = make(map[int64]struct{})
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
	// not enough targets
	if len(arena.mapRecord) < 2 {
		return
	}

	// get player arena section
	srcRec, ok := arena.mapRecord[id]
	if !ok {
		logger.Warning("cannot find player:", id, " 's arena record yet!")
		return
	}

	logger.Trace("Update Matching")

	secIdx := GetSectionIndexByScore(srcRec.ArenaScore)
	for k := range arena.listMatchPool[secIdx] {
		// peek self then continue
		if k == id {
			continue
		}

		dstRec, ok := arena.mapRecord[k]
		if !ok {
			continue
		}

		info := Instance().GetGameMgr().GetPlayerInfoByID(id)
		if info == nil {
			continue
		}

		if world := Instance().GetWorldSession().GetWorldByID(info.ServerId); world != nil {
			msg := &world_message.MUW_ArenaStartBattle{
				AttackId:     id,
				TargetRecord: dstRec,
			}
			world.SendProtoMessage(msg)
		}
	}
}

func (arena *Arena) UpdateRecordRequest() {
	for k := range arena.listRecReq {
		if _, ok := arena.mapRecord[k]; ok {
			arena.chMatchWait <- k

			arena.mu.Lock()
			delete(arena.listRecReq, k)
			arena.mu.Unlock()
		}
	}
}

func (arena *Arena) Matching(w *World, playerID int64) {
	_, ok := arena.mapRecord[playerID]
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
		arena.mu.Lock()
		arena.listRecReq[playerID] = struct{}{}
		arena.mu.Unlock()
	}
}

func (arena *Arena) AddRecord(rec *world_message.ArenaRecord) {
	if _, ok := arena.mapRecord[rec.PlayerId]; ok {
		return
	}

	arena.mu.Lock()

	// add to record
	arena.mapRecord[rec.PlayerId] = rec

	// add to slice record sorted by ArenaRecord
	arena.sliceRecord = append(arena.sliceRecord, rec)
	sort.Sort(arena.sliceRecord)

	// add to matching cache
	index := GetSectionIndexByScore(rec.ArenaScore)
	arena.listMatchPool[index][rec.PlayerId] = struct{}{}

	arena.mu.Unlock()

}

func (arena *Arena) ReorderRecord(rec *world_message.ArenaRecord, preSection, newSection int32) {
	arena.mu.Lock()

	delete(arena.listMatchPool[preSection], rec.PlayerId)
	arena.listMatchPool[newSection][rec.PlayerId] = struct{}{}

	arena.mu.Unlock()
}

func (arena *Arena) BattleResult(atkID int64, tarID int64, win bool) {
	logger.Info("arena battle result:", atkID, tarID, win)

	atkRec, ok := arena.mapRecord[atkID]
	if !ok {
		logger.Warning("arena battle result return without record:", atkID, tarID, win)
		return
	}

	preSection := GetSectionIndexByScore(atkRec.ArenaScore)
	atkRec.ArenaScore += 10
	newSection := GetSectionIndexByScore(atkRec.ArenaScore)
	if preSection != newSection {
		arena.ReorderRecord(atkRec, preSection, newSection)
	}
}
