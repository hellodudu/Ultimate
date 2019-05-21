package server

import (
	"errors"
	"fmt"
	"sync"

	"github.com/go-redis/redis"
	datastore "github.com/hellodudu/Ultimate/db"
	"github.com/hellodudu/Ultimate/game"
	"github.com/hellodudu/Ultimate/global"
	"github.com/hellodudu/Ultimate/iface"
	"github.com/hellodudu/Ultimate/logger"
	"github.com/hellodudu/Ultimate/task"
	"github.com/hellodudu/Ultimate/world"
)

// global var
var umt *ultimate

// ultimate define
type ultimate struct {
	td *task.Dispatcher // task dispatcher
	ds iface.IDatastore // datastore
	wm iface.IWorldMgr  // world manager
	gm iface.IGameMgr   // game manager
	mp iface.IMsgParser // msg parser

	rds      *redis.Client // redis
	tcpServ  *TcpServer    // tcp server
	rpcServ  *RpcServer    // rpc server
	httpServ *HttpServer   // http server
	reqNum   int           // request number
	appMap   map[int]*App  // app map
	wg       sync.WaitGroup
}

func NewUltimate() (iface.IUltimate, error) {
	if umt != nil {
		return umt, nil
	}

	umt = &ultimate{
		reqNum: 0,
		appMap: make(map[int]*App),
	}

	if ok := logger.Init(global.Debugging); !ok {
		return nil, errors.New("init log file failed")
	}

	umt.InitDatastore()
	umt.InitTask()
	umt.InitWorldMgr()
	umt.InitGame()
	umt.InitMsgParser()
	umt.InitTCPServer()
	umt.InitRPCServer()
	umt.InitHttpServer()

	logger.Print("all init ok!")
	return umt, nil
}

func (umt *ultimate) WorldMgr() iface.IWorldMgr {
	return umt.wm
}

func (umt *ultimate) GameMgr() iface.IGameMgr {
	return umt.gm
}

func (umt *ultimate) Datastore() iface.IDatastore {
	return umt.ds
}

// init task and taskdispatcher
func (umt *ultimate) InitTask() {
	var err error
	if umt.td, err = task.NewDispatcher(); err != nil {
		logger.Fatal(err)
		return
	}

	logger.Print("task init ok!")
}

// init db
func (umt *ultimate) InitDatastore() {
	var err error
	if umt.ds, err = datastore.NewDatastore(); err != nil {
		logger.Fatal(err)
		return
	}
	logger.Print("db_mgr init ok!")
}

func (umt *ultimate) InitRedis() {
	defer umt.wg.Done()
	umt.rds = redis.NewClient(&redis.Options{
		Addr:     global.RedisAddr,
		Password: global.RedisPwd,
		DB:       global.RedisDB,
	})

	if _, err := umt.rds.Ping().Result(); err != nil {
		logger.Fatal(err)
		return
	}

	logger.Print("redis init ok")
}

func (umt *ultimate) InitMsgParser() {
	var err error
	if umt.mp, err = NewMsgParser(umt.gm, umt.wm); err != nil {
		logger.Fatal(err)
	}

	logger.Print("msg parser init ok!")
}

// InitTCPServer init
func (umt *ultimate) InitTCPServer() {
	var err error
	if umt.tcpServ, err = NewTcpServer(umt.mp); err != nil {
		logger.Fatal(err)
	}

	logger.Print("tcp_server init ok!")
}

// InitRPCServer init
func (umt *ultimate) InitRPCServer() {
	defer umt.wg.Done()
	var err error
	if umt.rpcServ, err = NewRpcServer(); err != nil {
		logger.Fatal(err)
	}

	logger.Print("rpc_server init ok!")
}

// init http server
func (umt *ultimate) InitHttpServer() {
	defer umt.wg.Done()
	var err error
	if umt.httpServ, err = NewHttpServer(); err != nil {
		logger.Fatal(err)
	}

	logger.Print("http_server init ok!")
}

// init world session
func (umt *ultimate) InitWorldMgr() {
	var err error
	if umt.wm, err = world.NewWorldMgr(umt.ds); err != nil {
		logger.Fatal(err)
	}

	logger.Print("world_session init ok!")
}

func (umt *ultimate) InitGame() {
	var err error
	if umt.gm, err = game.NewGameMgr(umt.wm, umt.ds); err != nil {
		logger.Fatal(err)
	}

	logger.Print("gm init ok!")
}

// run
func (umt *ultimate) Run() {
	go umt.tcpServ.Run()
	go umt.rpcServ.Run()
	go umt.httpServ.Run()
	go umt.wm.Run()
	go umt.gm.Run()
	go umt.ds.Run()

}

func (umt *ultimate) Stop() {
	umt.rpcServ.Stop()
	umt.tcpServ.Stop()
	<-umt.ds.Stop()
	<-umt.wm.Stop()
}

func (umt *ultimate) GenReqNum() int {
	umt.wg.Add(1)
	umt.reqNum = umt.reqNum + 1
	umt.wg.Done()
	return umt.reqNum
}

func (umt *ultimate) AddTask(cb task.TaskCallback) {
	newReqNum := umt.GenReqNum()
	newTask, err := task.NewTask(newReqNum, cb)
	if err != nil {
		logger.Fatal("create new task error")
	}
	umt.td.AddTask(newTask)
}

func (umt *ultimate) AddNewApp(app *App) error {
	if _, ok := umt.appMap[app.AppID]; ok {
		errStr := fmt.Sprintf("add exist app<%d>\n", app.AppID)
		logger.Warning(errStr)
		return errors.New(errStr)
	}

	// todo insert into db
	// if umt.db == nil {
	// 	errStr := "db didn't exist!"
	// 	log.Println(errStr)
	// 	return errors.New(errStr)
	// }

	// stmt, err := umt.db.Prepare("insert into app values(?, ?, ?, ?)")
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

	umt.appMap[app.AppID] = app

	return nil
}
