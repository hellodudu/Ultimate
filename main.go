package main

import (
	"database/sql"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"sync"

	_ "github.com/go-sql-driver/mysql"
	"github.com/hellodudu/comment/comt"
	"github.com/hellodudu/comment/task"
)

var td *task.TaskDispatcher
var db *sql.DB
var ReqNum int = 0
var appMap map[int]comt.App

func taskHandler(w http.ResponseWriter, r *http.Request) {
	ReqNum = ReqNum + 1
	tk := &task.Task{Req: ReqNum}
	td.AddTask(tk)

	w.Write([]byte("It is done!"))
}

func createAppHandler(w http.ResponseWriter, r *http.Request) {

	body, err := ioutil.ReadAll(r.Body)
	if err != nil {
		log.Fatal(err)
	}

	newApp := &comt.App{}
	if err := json.Unmarshal(body, newApp); err != nil {
		log.Fatal(err)
	}

	// appid exist
	if _, ok := appMap[newApp.AppID]; ok {
		retBuf := fmt.Sprintf("appid<%d> exist!\n", newApp.AppID)
		log.Println(retBuf)
		w.Write([]byte(retBuf))
		return
	}

	// insert into db
	if db == nil {
		log.Println("db didn't exist!")
		return
	}

	stmt, err := db.Prepare("insert into app values(?, ?, ?, ?)")
	if err != nil {
		log.Fatal(err)
	}

	res, err := stmt.Exec(newApp.AppID, newApp.AppName, newApp.PubKey, newApp.PriKey)
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

	// add to app map cache
	log.Printf("create new app %+v!\n", newApp)
	appMap[newApp.AppID] = *newApp

	retBuf, retErr := json.Marshal(newApp)
	if retErr != nil {
		log.Fatal("create app response json marshal error!")
	}

	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusOK)
	w.Write(retBuf)
}

// db init
func dbInit(wg *sync.WaitGroup) {

	rows, rowErr := db.Query("select * from app")
	if rowErr != nil {
		log.Fatal(rowErr)
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
	}
	rowErr = rows.Err()
	if rowErr != nil {
		log.Fatal(rowErr)
	}

	wg.Done()
}

func main() {
	var wg sync.WaitGroup

	var err error
	if td, err = task.NewTaskDispatcher(); err != nil {
		panic("new task dispatcher failed!")
	}

	db, err = sql.Open("mysql", "root:hello1986@tcp(127.0.0.1:3306)/comt")
	if err != nil {
		log.Fatal(err)
	}
	defer db.Close()
	fmt.Printf("mysql connect success: %+v\n", db)

	wg.Add(1)
	go dbInit(&wg)

	wg.Wait()
	fmt.Println("all init ok!")
	appMap = make(map[int]comt.App)

	http.HandleFunc("/create_app", createAppHandler)
	http.HandleFunc("/task", taskHandler)
	log.Fatal(http.ListenAndServe(":8080", nil))

}
