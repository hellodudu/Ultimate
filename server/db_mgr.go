package ultimate

import (
	"context"
	"database/sql"
	"fmt"
	"log"
	"sync"
	"time"

	"github.com/fatih/color"
	"github.com/hellodudu/Ultimate/global"
)

type DBMgr struct {
	db      *sql.DB
	chWrite chan string
	ctx     context.Context
	cancel  context.CancelFunc
	wg      sync.WaitGroup
	chStop  chan struct{}
}

func NewDBMgr() (*DBMgr, error) {
	dbMgr := &DBMgr{
		chWrite: make(chan string, 100),
		chStop:  make(chan struct{}, 1),
	}

	dbMgr.ctx, dbMgr.cancel = context.WithCancel(context.Background())

	mysqlDSN := fmt.Sprintf("%s:%s@tcp(%s:%s)/%s", global.MysqlUser, global.MysqlPwd, global.MysqlAddr, global.MysqlPort, global.MysqlDB)
	var err error
	dbMgr.db, err = sql.Open("mysql", mysqlDSN)
	if err != nil {
		log.Fatal(err)
		return nil, err
	}

	dbMgr.InitDBMgr()
	return dbMgr, nil
}

func (m *DBMgr) Run() {
	for {
		select {
		case <-m.ctx.Done():
			log.Println(color.CyanString("db mgr context done!"))
			m.chStop <- struct{}{}
			return
		}
	}

}

func (m *DBMgr) Stop() chan struct{} {
	m.db.Close()
	m.cancel()
	return m.chStop
}

func (m *DBMgr) InitDBMgr() {

	m.wg.Add(1)
	go m.LoadGlobal()

	m.wg.Wait()

}

func (m *DBMgr) LoadGlobal() {
	defer m.wg.Done()

	query := "select * from global"
	stmt, err := m.db.PrepareContext(m.ctx, query)
	if err != nil {
		log.Println(color.YellowString("api initdb failed:", err.Error()))
		return
	}

	rows, err := stmt.QueryContext(m.ctx)
	if err != nil {
		log.Println(color.YellowString("api initdb failed:", err.Error()))
		return
	}

	if !rows.Next() {
		query = fmt.Sprintf("replace into global set id=%d, time_stamp=%d, arena_end_time=%d", global.UltimateID, int32(time.Now().Unix()), 0)
		if stmp, err := m.db.PrepareContext(m.ctx, query); err == nil {
			if _, err := stmp.ExecContext(m.ctx); err == nil {
				log.Println(color.CyanString("sql global init query exec success:", query))
			}
		}
	}
}

func (m *DBMgr) Exec(q string) {
	go func() {
		if _, err := m.db.ExecContext(m.ctx, q); err != nil {
			log.Println(color.RedString("db query failed:", err.Error()))
		}
	}()
}

func (m *DBMgr) Query(q string) (*sql.Rows, error) {
	return func() (*sql.Rows, error) {
		stmt, err := m.db.PrepareContext(m.ctx, q)
		if err != nil {
			log.Println(color.YellowString("db query<%s> failed:", q, err.Error()))
			return nil, err
		}

		rows, err := stmt.QueryContext(m.ctx)
		if err != nil {
			log.Println(color.YellowString("db query<%s> failed:", q, err.Error()))
			return nil, err
		}

		return rows, nil
	}()
}
