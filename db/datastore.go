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
}

func NewDatastore() (iface.IDatastore, error) {
	datastore := &Datastore{
		chStop: make(chan struct{}, 1),
		chExec: make(chan string, 1000),
	}

	datastore.ctx, datastore.cancel = context.WithCancel(context.Background())

	mysqlDSN := fmt.Sprintf("%s:%s@(%s:%s)/%s", global.MysqlUser, global.MysqlPwd, global.MysqlAddr, global.MysqlPort, global.MysqlDB)
	var err error
	datastore.db, err = gorm.Open("mysql", mysqlSDN)
	if err != nil {
		logger.Fatal(err)
		return nil, err
	}

	datastore.initDatastore()
	return datastore, nil
}

func (m *Datastore) Run() {
	for {
		select {
		case <-m.ctx.Done():
			logger.Print("db mgr context done!")
			m.chStop <- struct{}{}
			return
		case query := <-m.chExec:
			if _, err := m.db.ExecContext(m.ctx, query); err != nil {
				logger.Error(fmt.Sprintf("db exec<%s> failed:", query), err)
			}
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
	query := "select * from global"
	stmt, err := m.db.PrepareContext(m.ctx, query)
	if err != nil {
		logger.Warning("api initdb failed:", err)
		return
	}

	rows, err := stmt.QueryContext(m.ctx)
	if err != nil {
		logger.Warning("api initdb failed:", err)
		return
	}

	if !rows.Next() {
		query = fmt.Sprintf("replace into global set id=%d, time_stamp=%d, arena_season=%d, arena_week_end_time=%d, arena_season_end_time=%d", global.UltimateID, int32(time.Now().Unix()), 0, 0, 0)
		if stmp, err := m.db.PrepareContext(m.ctx, query); err == nil {
			if _, err := stmp.ExecContext(m.ctx); err == nil {
				logger.Info("sql global init query exec success:", query)
			}
		}
	}
}

func (m *Datastore) Exec(q string) {
	m.chExec <- q
}

func (m *Datastore) Query(q string) (*sql.Rows, error) {
	return func() (*sql.Rows, error) {
		stmt, err := m.db.PrepareContext(m.ctx, q)
		if err != nil {
			logger.Warning(fmt.Sprintf("db query<%s> failed:", q), err)
			return nil, err
		}

		rows, err := stmt.QueryContext(m.ctx)
		if err != nil {
			logger.Warning(fmt.Sprintf("db query<%s> failed:", q), err)
			return nil, err
		}

		return rows, nil
	}()
}
