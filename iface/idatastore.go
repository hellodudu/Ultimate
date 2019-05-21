package iface

import "database/sql"

type IDatastore interface {
	Exec(q string)
	Query(q string) (*sql.Rows, error)
	Run()
	Stop() chan struct{}
}
