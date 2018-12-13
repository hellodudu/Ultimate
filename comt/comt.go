package comt

import (
	"database/sql"
	"log"
	"sync"
	"time"

	"github.com/hellodudu/comment/task"
)

// api
type ComtAPI struct {
	td     *task.TaskDispatcher // task dispatcher
	db     *sql.DB              // database
	ReqNum int                  // request number
	appMap map[int]App          // app map
	wg     sync.WaitGroup
}

type Comt struct {
	ComtID   int64     `json:"id"`        // AppID(16) + TopicID(16) + commentid(32)
	AutherID int64     `json:"auther_id"` // player id
	Auther   string    `json:"auther"`    // player name
	Text     string    `json:"text"`      // comment text
	Evaluate int       `json:"evaluate"`  // 1-5 star
	GenTime  time.Time `json:"time"`
}

type Topic struct {
	TopicID int `json:"id"`
}

type App struct {
	AppID   int    `json:"id"`
	AppName string `json:"name"`
	PubKey  string `json:"pub_key"`
	PriKey  string `json:"pri_key"`
}

func NewComtAPI() (*ComtAPI, error) {
	api := &ComtAPI{
		ReqNum: 0,
		appMap: make(map[int]App),
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
	if api.td, err = task.NewTaskDispatcher(); err != nil {
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

	var (
		appid   int
		appname string
		pubkey  string
		prikey  string
	)
	for rows.Next() {
		if err := rows.Scan(&appid, &appname, &pubkey, &prikey); err != nil {
			log.Fatal(err)
		}
		log.Println("select result:", appid, appname, pubkey, prikey)

		// todo add to appMap
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

func (api *ComtAPI) AddTask() {
	api.wg.Add(1)
	api.ReqNum = api.ReqNum + 1
	newReqNum := api.ReqNum
	api.wg.Done()

	api.wg.Wait()
	api.td.AddTask(&task.Task{Req: newReqNum})
}
