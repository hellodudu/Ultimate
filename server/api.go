package server

import (
	"errors"
	"fmt"
	"sync"

	"github.com/go-redis/redis"
	datastore "github.com/hellodudu/Ultimate/db"
	"github.com/hellodudu/Ultimate/global"
	"github.com/hellodudu/Ultimate/iface"
	"github.com/hellodudu/Ultimate/logger"
	"github.com/hellodudu/Ultimate/task"
)

// global var
var api *API

// API define
type API struct {
	td *task.Dispatcher // task dispatcher
	ds iface.IDatastore // datastore
	wm iface.IWorldMgr  // world manager
	gm iface.IGameMgr   // game manager

	rds      *redis.Client // redis
	tcpServ  *TcpServer    // tcp server
	rpcServ  *RpcServer    // rpc server
	httpServ *HttpServer   // http server
	reqNum   int           // request number
	appMap   map[int]*App  // app map
	wg       sync.WaitGroup
}

func NewAPI() (iface.IApi, error) {
	if api != nil {
		return api, nil
	}

	api = &API{
		reqNum: 0,
		appMap: make(map[int]*App),
	}

	if ok := logger.Init(global.Debugging); !ok {
		return nil, errors.New("init log file failed")
	}

	api.wg.Add(6)
	go api.InitTask()
	go api.InitDatastore()
	// go api.InitRedis()
	go api.InitTCPServer()
	go api.InitRPCServer()
	go api.InitHttpServer()
	go api.InitWorldMgr()
	api.wg.Wait()

	// game init after db init ok!
	api.wg.Add(1)
	go api.InitGame()
	api.wg.Wait()

	logger.Print("all init ok!")
	return api, nil
}

func Instance() *API {
	return api
}

func (api *API) WorldMgr() iface.IWorldMgr {
	return api.wm
}

func (api *API) GameMgr() iface.IGameMgr {
	return api.gm
}

func (api *API) Datastore() iface.IDatastore {
	return api.ds
}

// init task and taskdispatcher
func (api *API) InitTask() {
	defer api.wg.Done()
	var err error
	if api.td, err = task.NewDispatcher(); err != nil {
		logger.Fatal(err)
		return
	}

	logger.Print("task init ok!")
}

// init db
func (api *API) InitDatastore() {
	defer api.wg.Done()
	var err error

	if api.ds, err = datastore.NewDatastore(); err != nil {
		logger.Fatal(err)
		return
	}

	logger.Print("db_mgr init ok!")
}

func (api *API) InitRedis() {
	defer api.wg.Done()
	api.rds = redis.NewClient(&redis.Options{
		Addr:     global.RedisAddr,
		Password: global.RedisPwd,
		DB:       global.RedisDB,
	})

	if _, err := api.rds.Ping().Result(); err != nil {
		logger.Fatal(err)
		return
	}

	logger.Print("redis init ok")
}

// InitTCPServer init
func (api *API) InitTCPServer() {
	defer api.wg.Done()
	var err error
	if api.tcpServ, err = NewTcpServer(); err != nil {
		logger.Fatal(err)
	}

	logger.Print("tcp_server init ok!")
}

// InitRPCServer init
func (api *API) InitRPCServer() {
	defer api.wg.Done()
	var err error
	if api.rpcServ, err = NewRpcServer(); err != nil {
		logger.Fatal(err)
	}

	logger.Print("rpc_server init ok!")
}

// init http server
func (api *API) InitHttpServer() {
	defer api.wg.Done()
	var err error
	if api.httpServ, err = NewHttpServer(); err != nil {
		logger.Fatal(err)
	}

	logger.Print("http_server init ok!")
}

// init world session
func (api *API) InitWorldMgr() {
	defer api.wg.Done()
	var err error
	if api.wm, err = NewWorldMgr(); err != nil {
		logger.Fatal(err)
	}

	logger.Print("world_session init ok!")
}

func (api *API) InitGame() {
	defer api.wg.Done()
	var err error
	if api.gm, err = NewGameMgr(); err != nil {
		logger.Fatal(err)
	}

	logger.Print("gm init ok!")
}

// run
func (api *API) Run() {
	go api.tcpServ.Run()
	go api.rpcServ.Run()
	go api.httpServ.Run()
	go api.wm.Run()
	go api.gm.Run()
	go api.ds.Run()

}

func (api *API) Stop() {
	api.rpcServ.Stop()
	api.tcpServ.Stop()
	<-api.ds.Stop()
	<-api.wm.Stop()
}

func (api *API) GenReqNum() int {
	api.wg.Add(1)
	api.reqNum = api.reqNum + 1
	api.wg.Done()
	return api.reqNum
}

func (api *API) AddTask(cb task.TaskCallback) {
	newReqNum := api.GenReqNum()
	newTask, err := task.NewTask(newReqNum, cb)
	if err != nil {
		logger.Fatal("create new task error")
	}
	api.td.AddTask(newTask)
}

func (api *API) AddNewApp(app *App) error {
	if _, ok := api.appMap[app.AppID]; ok {
		errStr := fmt.Sprintf("add exist app<%d>\n", app.AppID)
		logger.Warning(errStr)
		return errors.New(errStr)
	}

	// todo insert into db
	// if api.db == nil {
	// 	errStr := "db didn't exist!"
	// 	log.Println(errStr)
	// 	return errors.New(errStr)
	// }

	// stmt, err := api.db.Prepare("insert into app values(?, ?, ?, ?)")
	// if err != nil {
	// 	log.Fatal(err)
	// }

	// res, err := stmt.Exec(app.AppID, app.AppName, app.PubKey, app.PriKey)
	// if err != nil {
	// 	log.Fatal(err)
	// }

	// lastID, err := res.LastInsertId()
	// if err != nil {
	// 	log.Fatal(err)
	// }

	// rowAffect, err := res.RowsAffected()
	// if err != nil {
	// 	log.Fatal(err)
	// }

	// log.Printf("insert id = %d, affect rows = %d!\n", lastID, rowAffect)

	api.appMap[app.AppID] = app

	return nil
}
