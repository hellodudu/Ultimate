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
var ultimateAPI *UltimateAPI = nil

// UltimateAPI api define
type UltimateAPI struct {
	td       *task.Dispatcher // task dispatcher
	db       *sql.DB          // database
	rds      *redis.Client    // redis
	tcp_s    *TcpServer       // tcp server
	http_s   *HttpServer      // http server
	world_sn *WorldSession    // world session
	reqNum   int              // request number
	appMap   map[int]*App     // app map
	wg       sync.WaitGroup
	cWrite   chan string
}

func NewUltimateAPI() (*UltimateAPI, error) {
	if ultimateAPI != nil {
		return ultimateAPI, nil
	}

	ultimateAPI = &UltimateAPI{
		reqNum: 0,
		appMap: make(map[int]*App),
		cWrite: make(chan string, 100),
	}

	ultimateAPI.wg.Add(5)
	go ultimateAPI.InitTask()
	go ultimateAPI.InitDB()
	// go ultimateAPI.InitRedis()
	go ultimateAPI.InitTcpServer()
	go ultimateAPI.InitHttpServer()
	go ultimateAPI.InitWorldSession()

	ultimateAPI.wg.Wait()
	log.Println(color.CyanString("UltimateAPI all init ok!"))
	return ultimateAPI, nil
}

func GetUltimateAPI() *UltimateAPI {
	return ultimateAPI
}

func (api *UltimateAPI) GetWorldSession() *WorldSession {
	return api.world_sn
}

// init task and taskdispatcher
func (api *UltimateAPI) InitTask() {
	defer api.wg.Done()
	var err error
	if api.td, err = task.NewDispatcher(); err != nil {
		log.Fatal(err)
	}

	log.Println(color.CyanString("UltimateAPI task init ok!"))
}

// init db
func (api *UltimateAPI) InitDB() {
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

	log.Printf(color.CyanString("UltimateAPI db init ok!"))
}

func (api *UltimateAPI) InitRedis() {
	defer api.wg.Done()
	api.rds = redis.NewClient(&redis.Options{
		Addr:     config.RedisAddr,
		Password: config.RedisPwd,
		DB:       config.RedisDB,
	})

	if _, err := api.rds.Ping().Result(); err != nil {
		log.Fatal(err)
	}

	log.Println(color.CyanString("UltimateAPI redis init ok"))
}

// init tcp server
func (api *UltimateAPI) InitTcpServer() {
	defer api.wg.Done()
	var err error
	if api.tcp_s, err = NewTcpServer(); err != nil {
		log.Fatal(err)
	}

	log.Println(color.CyanString("UltimateAPI tcp_server init ok!"))
}

// init http server
func (api *UltimateAPI) InitHttpServer() {
	defer api.wg.Done()
	var err error
	if api.http_s, err = NewHttpServer(); err != nil {
		log.Fatal(err)
	}

	log.Println(color.CyanString("UltimateAPI http_server init ok!"))
}

// init world session
func (api *UltimateAPI) InitWorldSession() {
	defer api.wg.Done()
	var err error
	if api.world_sn, err = NewWorldSession(); err != nil {
		log.Fatal(err)
	}

	log.Println(color.CyanString("UltimateAPI world_session init ok!"))
}

// run
func (api *UltimateAPI) Run() {
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

func (api *UltimateAPI) Stop() {
	api.db.Close()
	api.world_sn.Stop()
	os.Exit(0)
}

func (api *UltimateAPI) GenReqNum() int {
	api.wg.Add(1)
	api.reqNum = api.reqNum + 1
	api.wg.Done()
	return api.reqNum
}

func (api *UltimateAPI) AddTask(cb task.TaskCallback) {
	newReqNum := api.GenReqNum()
	newTask, err := task.NewTask(newReqNum, cb)
	if err != nil {
		log.Fatal("create new task error")
	}
	api.td.AddTask(newTask)
}

func (api *UltimateAPI) AddNewApp(app *App) error {
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

func (api *UltimateAPI) QueryWrite(query string) {
	api.cWrite <- query
}
