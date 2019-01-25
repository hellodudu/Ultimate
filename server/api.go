package ultimate

import (
	"database/sql"
	"errors"
	"fmt"
	"log"
	"os"
	"sync"

	"github.com/fatih/color"
	"github.com/go-redis/redis"
	"github.com/hellodudu/Ultimate/config"
	"github.com/hellodudu/Ultimate/task"
)

// global var
var api *API = nil

// api define
type API struct {
	td       *task.Dispatcher // task dispatcher
	db       *sql.DB          // database
	rds      *redis.Client    // redis
	tcp_s    *TcpServer       // tcp server
	http_s   *HttpServer      // http server
	world_sn *WorldSession    // world session
	gameMgr  *GameMgr
	reqNum   int          // request number
	appMap   map[int]*App // app map
	wg       sync.WaitGroup
	cWrite   chan string
}

func NewAPI() (*API, error) {
	if api != nil {
		return api, nil
	}

	api = &API{
		reqNum: 0,
		appMap: make(map[int]*App),
		cWrite: make(chan string, 100),
	}

	api.wg.Add(6)
	go api.InitTask()
	go api.InitDB()
	// go api.InitRedis()
	go api.InitTcpServer()
	go api.InitHttpServer()
	go api.InitWorldSession()
	go api.InitGame()

	api.wg.Wait()
	log.Println(color.CyanString("api all init ok!"))
	return api, nil
}

func Instance() *API {
	return api
}

func (api *API) GetWorldSession() *WorldSession {
	return api.world_sn
}

func (api *API) GetGameMgr() *GameMgr {
	return api.gameMgr
}

// init task and taskdispatcher
func (api *API) InitTask() {
	defer api.wg.Done()
	var err error
	if api.td, err = task.NewDispatcher(); err != nil {
		log.Fatal(err)
	}

	log.Println(color.CyanString("api task init ok!"))
}

// init db
func (api *API) InitDB() {
	defer api.wg.Done()
	var err error
	api.db, err = sql.Open("mysql", config.MysqlDSN)
	if err != nil {
		log.Fatal(err)
	}

	// rows, err := api.db.Query("select * from app")
	// if err != nil {
	// 	log.Fatal(err)
	// }
	// defer rows.Close()

	// newApp := &App{}
	// for rows.Next() {
	// 	if err := rows.Scan(&newApp.AppID, &newApp.AppName, &newApp.PubKey, &newApp.PriKey); err != nil {
	// 		log.Fatal(err)
	// 	}
	// 	log.Println("select result:", newApp)

	// 	// add to appMap
	// 	api.appMap[newApp.AppID] = newApp
	// }

	// if err := rows.Err(); err != nil {
	// 	log.Fatal(err)
	// }

	log.Printf(color.CyanString("api db init ok!"))
}

func (api *API) InitRedis() {
	defer api.wg.Done()
	api.rds = redis.NewClient(&redis.Options{
		Addr:     config.RedisAddr,
		Password: config.RedisPwd,
		DB:       config.RedisDB,
	})

	if _, err := api.rds.Ping().Result(); err != nil {
		log.Fatal(err)
	}

	log.Println(color.CyanString("api redis init ok"))
}

// init tcp server
func (api *API) InitTcpServer() {
	defer api.wg.Done()
	var err error
	if api.tcp_s, err = NewTcpServer(); err != nil {
		log.Fatal(err)
	}

	log.Println(color.CyanString("api tcp_server init ok!"))
}

// init http server
func (api *API) InitHttpServer() {
	defer api.wg.Done()
	var err error
	if api.http_s, err = NewHttpServer(); err != nil {
		log.Fatal(err)
	}

	log.Println(color.CyanString("api http_server init ok!"))
}

// init world session
func (api *API) InitWorldSession() {
	defer api.wg.Done()
	var err error
	if api.world_sn, err = NewWorldSession(); err != nil {
		log.Fatal(err)
	}

	log.Println(color.CyanString("api world_session init ok!"))
}

func (api *API) InitGame() {
	defer api.wg.Done()
	var err error
	if api.gameMgr, err = NewGameMgr(); err != nil {
		log.Fatal(err)
	}

	log.Println(color.CyanString("api gameMgr init ok!"))
}

// run
func (api *API) Run() {
	go api.tcp_s.Run()
	go api.http_s.Run()
	go api.world_sn.Run()

	go func() {
		select {
		default:
		case q := <-api.cWrite:
			api.db.Exec(q)
		}
	}()
}

func (api *API) Stop() {
	api.db.Close()
	api.world_sn.Stop()
	os.Exit(0)
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
		log.Fatal("create new task error")
	}
	api.td.AddTask(newTask)
}

func (api *API) AddNewApp(app *App) error {
	if _, ok := api.appMap[app.AppID]; ok {
		errStr := fmt.Sprintf("add exist app<%d>\n", app.AppID)
		log.Printf(errStr)
		return errors.New(errStr)
	}

	// todo insert into db
	if api.db == nil {
		errStr := "db didn't exist!"
		log.Println(errStr)
		return errors.New(errStr)
	}

	stmt, err := api.db.Prepare("insert into app values(?, ?, ?, ?)")
	if err != nil {
		log.Fatal(err)
	}

	res, err := stmt.Exec(app.AppID, app.AppName, app.PubKey, app.PriKey)
	if err != nil {
		log.Fatal(err)
	}

	lastID, err := res.LastInsertId()
	if err != nil {
		log.Fatal(err)
	}

	rowAffect, err := res.RowsAffected()
	if err != nil {
		log.Fatal(err)
	}

	log.Printf("insert id = %d, affect rows = %d!\n", lastID, rowAffect)

	api.appMap[app.AppID] = app

	return nil
}

func (api *API) QueryWrite(query string) {
	api.cWrite <- query
}
