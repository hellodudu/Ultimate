package comt

import (
	"database/sql"
	"errors"
	"fmt"
	"log"
	"net/http"
	"sync"

	"github.com/hellodudu/comment/task"
)

// ComtAPI api define
type ComtAPI struct {
	td     *task.Dispatcher // task dispatcher
	db     *sql.DB          // database
	reqNum int              // request number
	appMap map[int]*App     // app map
	wg     sync.WaitGroup
}

func NewComtAPI() (*ComtAPI, error) {
	api := &ComtAPI{
		reqNum: 0,
		appMap: make(map[int]*App),
	}

	go api.InitTask()
	go api.InitDB()

	api.wg.Wait()
	log.Println("ComtAPI all init ok!")
	return api, nil
}

// init task and taskdispatcher
func (api *ComtAPI) InitTask() {
	api.wg.Add(1)
	var err error
	if api.td, err = task.NewDispatcher(); err != nil {
		log.Fatal(err)
	}

	api.wg.Done()
	log.Println("ComtAPI task init ok!")
}

// init db
func (api *ComtAPI) InitDB() {
	var err error
	api.wg.Add(1)
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

	api.wg.Done()
	log.Printf("ComtAPI db init ok!")
}

// defer close
func (api *ComtAPI) Close() {
	api.db.Close()
}

func (api *ComtAPI) GenReqNum() int {
	api.wg.Add(1)
	api.reqNum = api.reqNum + 1
	api.wg.Done()
	return api.reqNum
}

func (api *ComtAPI) AddTask() {
	newReqNum := api.GenReqNum()
	newTask, err := task.NewTask(newReqNum)
	if err != nil {
		log.Fatal("create new task error")
	}
	api.td.AddTask(newTask)
}

func (api *ComtAPI) AddHttpTask(w http.ResponseWriter, r *http.Request, cb task.TaskCallback) {
	newReqNum := api.GenReqNum()
	newTask, err := task.NewHttpTask(newReqNum, w, r, cb)
	if err != nil {
		log.Fatal("create new http task error")
	}
	api.td.AddTask(newTask)
}

func (api *ComtAPI) AddNewApp(app *App) error {
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
