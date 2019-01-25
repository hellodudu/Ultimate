package ultimate

import (
	"context"
	"log"
	"sync"

	"github.com/fatih/color"
	"github.com/hellodudu/Ultimate/proto"
)

type Arena struct {
	mapRecord    map[int64]*world_message.ArenaRecord // all player's arena record
	mapMatching  map[int64]struct{}                   // all matching id
	mapMatchPool map[int64]struct{}                   // match pool
	ctx          context.Context
	cancel       context.CancelFunc
	mu           sync.Mutex
}

func NewArena(ctx context.Context) (*Arena, error) {
	arena := &Arena{
		mapRecord:    make(map[int64]*world_message.ArenaRecord),
		mapMatching:  make(map[int64]struct{}),
		mapMatchPool: make(map[int64]struct{}),
	}

	arena.ctx, arena.cancel = context.WithCancel(ctx)
	return arena, nil
}

func (arena *Arena) Stop() {
	arena.cancel()
}

func (arena *Arena) Run() {
	for {
		select {
		// context canceled
		case <-arena.ctx.Done():
			log.Println(color.RedString("arena context done!"))
			return

		// matching
		default:
			arena.UpdateMatching()
		}
	}
}

func (arena *Arena) UpdateMatching() {
	if len(arena.mapMatching) < 2 {
		return
	}

	for k := range arena.mapMatchPool {
		r := arena.PeekTarget(k)
		if r == nil {
			continue
		}

		info := Instance().GetGameMgr().GetPlayerInfoByID(k)
		if info == nil {
			continue
		}

		if world := Instance().GetWorldSession().GetWorldByID(info.ServerId); world != nil {
			msg := &world_message.MUW_ArenaStartBattle{
				AttackId:     k,
				TargetRecord: r,
			}
			world.SendProtoMessage(msg)
		}

		delete(arena.mapMatchPool, k)
	}

}

func (arena *Arena) PeekTarget(playerID int64) *world_message.ArenaRecord {
	for k := range arena.mapMatching {
		if k != playerID {
			return arena.mapRecord[k]
		}
	}

	return nil
}

func (arena *Arena) Matching(w *World, playerID int64) {
	if _, ok := arena.mapMatching[playerID]; ok {
		return
	}

	// request player record
	msg := &world_message.MUW_ArenaAddRecord{
		PlayerId: playerID,
	}

	w.SendProtoMessage(msg)

	// add to match pool
	arena.mu.Lock()
	arena.mapMatchPool[playerID] = struct{}{}
	arena.mu.Unlock()
}

func (arena *Arena) AddRecord(w *World, rec *world_message.ArenaRecord) {
	if _, ok := arena.mapRecord[rec.PlayerId]; ok {
		return
	}

	arena.mu.Lock()

	// add to record
	arena.mapRecord[rec.PlayerId] = rec

	// add to matching cache
	arena.mapMatching[rec.PlayerId] = struct{}{}

	arena.mu.Unlock()
}

func (arena *Arena) BattleResult(atkID int64, tarID int64, win bool) {

}
