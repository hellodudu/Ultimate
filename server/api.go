package ultimate

import (
	"errors"
	"fmt"
	"sync"

	"github.com/go-redis/redis"
	"github.com/hellodudu/Ultimate/global"
	"github.com/hellodudu/Ultimate/logger"
	"github.com/hellodudu/Ultimate/task"
)

// global var
var api *API

// API define
type API struct {
	td        *task.Dispatcher // task dispatcher
	dbMgr     *DBMgr           // db manager
	rds       *redis.Client    // redis
	tcpServ   *TcpServer       // tcp server
	rpcServ   *RpcServer       // rpc server
	httpServ  *HttpServer      // http server
	worldSesn *WorldSession    // world session
	gameMgr   *GameMgr
	reqNum    int          // request number
	appMap    map[int]*App // app map
	wg        sync.WaitGroup
}

func NewAPI() (*API, error) {
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
	go api.InitDBMgr()
	// go api.InitRedis()
	go api.InitTCPServer()
	go api.InitRPCServer()
	go api.InitHttpServer()
	go api.InitWorldSession()
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

func (api *API) GetWorldSession() *WorldSession {
	return api.worldSesn
}

func (api *API) GetGameMgr() *GameMgr {
	return api.gameMgr
}

func (api *API) GetDBMgr() *DBMgr {
	return api.dbMgr
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
func (api *API) InitDBMgr() {
	defer api.wg.Done()
	var err error

	if api.dbMgr, err = NewDBMgr(); err != nil {
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
func (api *API) InitWorldSession() {
	defer api.wg.Done()
	var err error
	if api.worldSesn, err = NewWorldSession(); err != nil {
		logger.Fatal(err)
	}

	logger.Print("world_session init ok!")
}

func (api *API) InitGame() {
	defer api.wg.Done()
	var err error
	if api.gameMgr, err = NewGameMgr(); err != nil {
		logger.Fatal(err)
	}

	logger.Print("gameMgr init ok!")
}

// run
func (api *API) Run() {
	go api.tcpServ.Run()
	go api.rpcServ.Run()
	go api.httpServ.Run()
	go api.worldSesn.Run()
	go api.gameMgr.Run()
	go api.dbMgr.Run()

}

func (api *API) Stop() {
	api.rpcServ.Stop()
	<-api.dbMgr.Stop()
	<-api.worldSesn.Stop()
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
