package arena

import (
	"sort"
	"sync"
)

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

func (s *rankArenaData) length() int {
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

func (s *rankArenaData) get(n int) *arenaData {
	s.rwLock.RLock()
	defer s.rwLock.RUnlock()
	return s.item[n]
}

func (s *rankArenaData) getBottom() *arenaData {
	s.rwLock.RLock()
	defer s.rwLock.RUnlock()
	if s.Len() == 0 {
		return nil
	}
	return s.item[len(s.item)-1]
}

func (s *rankArenaData) getIndexBefore100(d *arenaData) int {
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

func (s *rankArenaData) getTop(top int) []*arenaData {
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

func (s *rankArenaData) getListByPage(page int) []*arenaData {
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
