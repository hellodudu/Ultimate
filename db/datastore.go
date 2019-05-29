package datastore

import (
	"context"
	"database/sql"
	"fmt"
	"time"

	"github.com/hellodudu/Ultimate/global"
	"github.com/hellodudu/Ultimate/iface"
	"github.com/hellodudu/Ultimate/logger"
	"github.com/jinzhu/gorm"
)

type Datastore struct {
	db     *gorm.DB
	ctx    context.Context
	cancel context.CancelFunc
	chStop chan struct{}
	chExec chan string

	// table
	global *iface.TableGlobal
}

func NewDatastore() (iface.IDatastore, error) {
	datastore := &Datastore{
		chStop: make(chan struct{}, 1),
		chExec: make(chan string, 1000),
	}

	datastore.ctx, datastore.cancel = context.WithCancel(context.Background())

	mysqlDSN := fmt.Sprintf("%s:%s@(%s:%s)/%s", global.MysqlUser, global.MysqlPwd, global.MysqlAddr, global.MysqlPort, global.MysqlDB)
	var err error
	datastore.db, err = gorm.Open("mysql", mysqlDSN)
	if err != nil {
		logger.Fatal(err)
		return nil, err
	}

	datastore.initDatastore()
	return datastore, nil
}

func (m *Datastore) DB() *gorm.DB {
	return m.db
}

func (m *Datastore) TableGlobal() *iface.TableGlobal {
	return m.global
}

func (m *Datastore) Run() {
	for {
		select {
		case <-m.ctx.Done():
			logger.Print("db mgr context done!")
			m.chStop <- struct{}{}
			return
		case _ = <-m.chExec:
			// if _, err := m.db.ExecContext(m.ctx, query); err != nil {
			// 	logger.Error(fmt.Sprintf("db exec<%s> failed:", query), err)
			// }
		}
	}

}

func (m *Datastore) Stop() chan struct{} {
	m.db.Close()
	m.cancel()
	return m.chStop
}

func (m *Datastore) initDatastore() {
	m.loadGlobal()
}

func (m *Datastore) loadGlobal() {
	m.global = &iface.TableGlobal{
		Id:                 global.UltimateID,
		TimeStamp:          int(int32(time.Now().Unix())),
		ArenaSeason:        0,
		ArenaWeekEndTime:   0,
		ArenaSeasonEndTime: 0,
	}

	m.db.AutoMigrate(m.global)
	if m.db.FirstOrCreate(m.global, global.UltimateID).RecordNotFound() {
		m.db.Create(m.global)
	}

	logger.Info("datastore loadGlobal success:", m.global)
}

func (m *Datastore) Exec(q string) {
	m.chExec <- q
}

func (m *Datastore) Query(q string) (*sql.Rows, error) {
	return nil, fmt.Errorf("test error")
	// return func() (*sql.Rows, error) {
	// 	stmt, err := m.db.PrepareContext(m.ctx, q)
	// 	if err != nil {
	// 		logger.Warning(fmt.Sprintf("db query<%s> failed:", q), err)
	// 		return nil, err
	// 	}

	// 	rows, err := stmt.QueryContext(m.ctx)
	// 	if err != nil {
	// 		logger.Warning(fmt.Sprintf("db query<%s> failed:", q), err)
	// 		return nil, err
	// 	}

	// 	return rows, nil
	// }()
}
