package ultimate

import (
	"database/sql"
	"errors"
	"fmt"
	"log"
	"net/http"
	"os"
	"sync"

	"github.com/hellodudu/comment/task"
)

// global var
var ultimateAPI *UltimateAPI = nil

// UltimateAPI api define
type UltimateAPI struct {
	td       *task.Dispatcher // task dispatcher
	db       *sql.DB          // database
	tcp_s    *TcpServer       // tcp server
	http_s   *HttpServer      // http server
	world_sn *WorldSession    // world session
	reqNum   int              // request number
	appMap   map[int]*App     // app map
	wg       sync.WaitGroup
}

func NewUltimateAPI() (*UltimateAPI, error) {
	if ultimateAPI != nil {
		return ultimateAPI, nil
	}

	ultimateAPI = &UltimateAPI{
		reqNum: 0,
		appMap: make(map[int]*App),
	}

	ultimateAPI.wg.Add(5)
	go ultimateAPI.InitTask()
	go ultimateAPI.InitDB()
	go ultimateAPI.InitTcpServer()
	go ultimateAPI.InitHttpServer()
	go ultimateAPI.InitWorldSession()

	ultimateAPI.wg.Wait()
	log.Println("UltimateAPI all init ok!")
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

	log.Println("UltimateAPI task init ok!")
}

// init db
func (api *UltimateAPI) InitDB() {
	defer api.wg.Done()
	var err error
	api.db, err = sql.Open("mysql", "root:hello1986@tcp(127.0.0.1:3306)/comt")
	if err != nil {
		log.Fatal(err)
	}

	rows, err := api.db.Query("select * from app")
	if err != nil {
		log.Fatal(err)
	}
	defer rows.Close()

	newApp := &App{}
	for rows.Next() {
		if err := rows.Scan(&newApp.AppID, &newApp.AppName, &newApp.PubKey, &newApp.PriKey); err != nil {
			log.Fatal(err)
		}
		log.Println("select result:", newApp)

		// add to appMap
		api.appMap[newApp.AppID] = newApp
	}

	if err := rows.Err(); err != nil {
		log.Fatal(err)
	}

	log.Printf("UltimateAPI db init ok!")
}

// init tcp server
func (api *UltimateAPI) InitTcpServer() {
	defer api.wg.Done()
	var err error
	if api.tcp_s, err = NewTcpServer(); err != nil {
		log.Fatal(err)
	}

	log.Println("UltimateAPI tcp_server init ok!")
}

// init http server
func (api *UltimateAPI) InitHttpServer() {
	defer api.wg.Done()
	var err error
	if api.http_s, err = NewHttpServer(); err != nil {
		log.Fatal(err)
	}

	log.Println("UltimateAPI http_server init ok!")
}

// init world session
func (api *UltimateAPI) InitWorldSession() {
	defer api.wg.Done()
	var err error
	if api.world_sn, err = NewWorldSession(); err != nil {
		log.Fatal(err)
	}

	log.Println("UltimateAPI world_session init ok!")
}

// run
func (api *UltimateAPI) Run() {
	go api.tcp_s.Run()
	go api.http_s.Run()
	go api.world_sn.Run()
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

func (api *UltimateAPI) AddTask() {
	newReqNum := api.GenReqNum()
	newTask, err := task.NewTask(newReqNum)
	if err != nil {
		log.Fatal("create new task error")
	}
	api.td.AddTask(newTask)
}

func (api *UltimateAPI) AddHttpTask(w http.ResponseWriter, r *http.Request, cb task.TaskCallback) {
	newReqNum := api.GenReqNum()
	newTask, err := task.NewHttpTask(newReqNum, w, r, cb)
	if err != nil {
		log.Fatal("create new http task error")
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
