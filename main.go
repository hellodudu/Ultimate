package main

import (
	"encoding/json"
	"io/ioutil"
	"log"
	"net/http"

	_ "github.com/go-sql-driver/mysql"
	"github.com/hellodudu/comment/comt"
)

var comtAPI *comt.ComtAPI

func taskHandler(w http.ResponseWriter, r *http.Request) {
	comtAPI.AddTask()
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
	// if _, ok := appMap[newApp.AppID]; ok {
	// 	retBuf := fmt.Sprintf("appid<%d> exist!\n", newApp.AppID)
	// 	log.Println(retBuf)
	// 	w.Write([]byte(retBuf))
	// 	return
	// }

	// // insert into db
	// if db == nil {
	// 	log.Println("db didn't exist!")
	// 	return
	// }

	// stmt, err := db.Prepare("insert into app values(?, ?, ?, ?)")
	// if err != nil {
	// 	log.Fatal(err)
	// }

	// res, err := stmt.Exec(newApp.AppID, newApp.AppName, newApp.PubKey, newApp.PriKey)
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

	// // add to app map cache
	// log.Printf("create new app %+v!\n", newApp)
	// appMap[newApp.AppID] = *newApp

	retBuf, retErr := json.Marshal(newApp)
	if retErr != nil {
		log.Fatal("create app response json marshal error!")
	}

	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusOK)
	w.Write(retBuf)
}

func main() {
	var err error
	if comtAPI, err = comt.NewComtAPI(); err != nil {
		log.Fatal(err)
	}
	defer comtAPI.Close()

	http.HandleFunc("/create_app", createAppHandler)
	http.HandleFunc("/task", taskHandler)
	log.Fatal(http.ListenAndServe(":8080", nil))

}
