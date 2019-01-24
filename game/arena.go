package game

import (
	"context"
	"log"
	"sync"

	"github.com/fatih/color"
	"github.com/hellodudu/Ultimate/proto"
	ultimate "github.com/hellodudu/Ultimate/server"
)

type Arena struct {
	mapRecord    map[int64]*world_message.ArenaRecord // all player's arena record
	mapMatching  map[int64]struct{}                   // all matching id
	mapMatchPool map[int64]*world_message.ArenaRecord // match pool
	ctx          context.Context
	cancel       context.CancelFunc
	mu           sync.Mutex
}

func NewArena(ctx context.Context) (*Arena, error) {
	arena = &Arena{
		mapRecord:    make(map[int64]*world_message.ArenaRecord),
		mapMatching:  make(map[int64]struct{}),
		mapMatchPool: make(map[int64]*world_message.ArenaRecord),
	}

	arena.ctx, arena.cancel = context.WithCancel(ctx)
	return arena, nil
}

func GetArena() *Arena {
	return arena
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
	for k, v := range arena.mapMatchPool {
		if v != nil {
			continue
		}

	}
}

func (arena *Arena) Matching(w *ultimate.World, playerID int64) {
	if _, ok := arena.mapMatching[playerID]; ok {
		return
	}

	// request player record
	msg := &world_message.MUW_ArenaAddRecord{
		player_id: playerID,
	}

	w.SendProtoMessage(msg)

	// add to match pool
	arena.mu.Lock()
	arena.mapMatchPool[playerID] = nil
	arena.mu.Unlock()
}

func (arena *Arena) AddRecord(w *ultimate.World, rec *world_message.ArenaRecord) {
	if _, ok := arena.mapRecord[rec.player_id]; ok {
		return
	}

	arena.mu.Lock()

	// add to record
	arena.mapRecord[rec.player_id] = rec

	// add to matching cache
	arena.mapMatching[rec.player_id] = struct{}{}

	arena.mu.Unlock()
}
