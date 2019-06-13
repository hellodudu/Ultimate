package datastore

import (
	"context"
	"fmt"

	"github.com/hellodudu/Ultimate/iface"
	"github.com/hellodudu/Ultimate/logger"
	"github.com/hellodudu/Ultimate/utils/global"
	"github.com/jinzhu/gorm"
)

type Datastore struct {
	db     *gorm.DB
	ctx    context.Context
	cancel context.CancelFunc
	chStop chan struct{}

	// table
	global *iface.TableGlobal
}

func NewDatastore() (*Datastore, error) {
	datastore := &Datastore{
		chStop: make(chan struct{}, 1),
	}

	datastore.ctx, datastore.cancel = context.WithCancel(context.Background())

	mysqlDSN := fmt.Sprintf("%s:%s@(%s:%s)/%s", global.MysqlUser, global.MysqlPwd, global.MysqlAddr, global.MysqlPort, global.MysqlDB)
	var err error
	datastore.db, err = gorm.Open("mysql", mysqlDSN)
	if err != nil {
		logger.Fatal(err)
		return nil, err
	}

	// datastore.db.LogMode(true)

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
			logger.Info("datastore context done!")
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
