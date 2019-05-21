package datastore

import (
	"context"
	"database/sql"
	"fmt"
	"sync"
	"time"

	"github.com/hellodudu/Ultimate/global"
	"github.com/hellodudu/Ultimate/iface"
	"github.com/hellodudu/Ultimate/logger"
)

type Datastore struct {
	db     *sql.DB
	ctx    context.Context
	cancel context.CancelFunc
	wg     sync.WaitGroup
	chStop chan struct{}
}

func NewDatastore() (iface.IDatastore, error) {
	datastore := &Datastore{
		chStop: make(chan struct{}, 1),
	}

	datastore.ctx, datastore.cancel = context.WithCancel(context.Background())

	mysqlDSN := fmt.Sprintf("%s:%s@(%s:%s)/%s", global.MysqlUser, global.MysqlPwd, global.MysqlAddr, global.MysqlPort, global.MysqlDB)
	var err error
	datastore.db, err = sql.Open("mysql", mysqlDSN)
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
		}
	}

}

func (m *Datastore) Stop() chan struct{} {
	m.db.Close()
	m.cancel()
	return m.chStop
}

func (m *Datastore) initDatastore() {

	m.wg.Add(1)
	go m.loadGlobal()

	m.wg.Wait()

}

func (m *Datastore) loadGlobal() {
	defer m.wg.Done()

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
	go func() {
		if _, err := m.db.ExecContext(m.ctx, q); err != nil {
			logger.Error(fmt.Sprintf("db exec<%s> failed:", q), err)
		}
	}()
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
