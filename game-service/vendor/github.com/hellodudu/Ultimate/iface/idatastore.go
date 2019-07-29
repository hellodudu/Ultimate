package iface

import (
	"github.com/jinzhu/gorm"
)

type IDatastore interface {
	DB() *gorm.DB
	TableGlobal() *TableGlobal
	Run()
	Stop()
}

// Global mysql table global
type TableGlobal struct {
	Id                 int `gorm:"type:int(10);primary_key;column:id;default:0;not null"`
	TimeStamp          int `gorm:"type:int(10);column:time_stamp;default:0;not null"`
	ArenaSeason        int `gorm:"type:int(10);column:arena_season;default:0;not null"`
	ArenaWeekEndTime   int `gorm:"type:int(10);column:arena_week_end_time;default:0;not null"` // every monday request new player's record and send weekly reward
	ArenaSeasonEndTime int `gorm:"type:int(10);column:arena_season_end_time;default:0;not null"`
}

// TableName set global table name to be `global`
func (TableGlobal) TableName() string {
	return "global"
}
