package ultimate

import (
	"encoding/json"
	"expvar"
	"fmt"
	"io/ioutil"
	"net/http"
	_ "net/http/pprof"
	"runtime"
	"time"

	"github.com/hellodudu/Ultimate/global"
	"github.com/hellodudu/Ultimate/logger"
)

var testChan = make(chan interface{}, 1)
var startTime = time.Now()
var lastGCPause uint32

func calculateUptime() interface{} {
	return time.Since(startTime).String()
}

func currentGoVersion() interface{} {
	return runtime.Version()
}

func getNumCPUs() interface{} {
	return runtime.NumCPU()
}

func getGoOS() interface{} {
	return runtime.GOOS
}

func getNumGoroutins() interface{} {
	return runtime.NumGoroutine()
}

func getLastGCPauseTime() interface{} {
	var gcPause uint64
	ms := new(runtime.MemStats)

	statString := expvar.Get("memstats").String()
	if statString != "" {
		json.Unmarshal([]byte(statString), ms)

		if lastGCPause == 0 || lastGCPause != ms.NumGC {
			gcPause = ms.PauseNs[(ms.NumGC+255)%256]
			lastGCPause = ms.NumGC
		}
	}

	return gcPause
}

func getArenaPlayerDataNum() interface{} {
	return Instance().GetGameMgr().GetArena().GetArenaDataNum()
}

func getArenaRecordNum() interface{} {
	return Instance().GetGameMgr().GetArena().GetRecordNum()
}

type HttpServer struct {
}

func NewHttpServer() (*HttpServer, error) {
	return &HttpServer{}, nil
}

func (server *HttpServer) Run() {
	http.HandleFunc("/create_app", createAppHandler)
	http.HandleFunc("/task", taskHandler)
	http.HandleFunc("/ws", wsHandler)
	http.HandleFunc("/bn", binaryHandler)

	expvar.Publish("ticktime", expvar.Func(calculateUptime))
	expvar.Publish("version", expvar.Func(currentGoVersion))
	expvar.Publish("cores", expvar.Func(getNumCPUs))
	expvar.Publish("os", expvar.Func(getGoOS))
	expvar.Publish("goroutine", expvar.Func(getNumGoroutins))
	expvar.Publish("gcpause", expvar.Func(getLastGCPauseTime))
	expvar.Publish("arena_player_data_num", expvar.Func(getArenaPlayerDataNum))
	expvar.Publish("arena_record_num", expvar.Func(getArenaRecordNum))

	http.HandleFunc("/arena_get_player_data", arenaGetPlayerDataHandler)
	http.HandleFunc("/arena_matching_list", arenaMatchingListHandler)
	http.HandleFunc("/arena_record_req_list", arenaRecordReqListHandler)
	http.HandleFunc("/arena_get_record", arenaGetRecordHandler)
	http.HandleFunc("/player_info", getPlayerInfoHandler)
	http.HandleFunc("/guild_info", getGuildInfoHandler)

	addr, err := global.IniMgr.GetIniValue("config/ultimate.ini", "listen", "HttpListenAddr")
	if err != nil {
		logger.Error("cannot read ini HttpListenAddr!")
		return
	}

	logger.Error(http.ListenAndServe(addr, nil))

}

func taskHandler(w http.ResponseWriter, r *http.Request) {
	c := make(chan struct{}, 1)
	Instance().AddTask(func() {
		w.Write([]byte("taskHandler Callback!"))
		c <- struct{}{}
	})
	<-c
}

func wsHandler(w http.ResponseWriter, r *http.Request) {
	conn, err := global.Upgrader.Upgrade(w, r, nil)
	if err != nil {
		logger.Error(err)
	}

	for {
		msgtype, p, err := conn.ReadMessage()
		if err != nil {
			logger.Warning(err)
			return
		}

		res := []byte("server recv:")
		if err := conn.WriteMessage(msgtype, append(res, p...)); err != nil {
			logger.Warning(err)
			return
		}
	}
}

func createAppHandler(w http.ResponseWriter, r *http.Request) {

	body, err := ioutil.ReadAll(r.Body)
	if err != nil {
		logger.Warning(err)
	}

	newApp := &App{}
	if err := json.Unmarshal(body, newApp); err != nil {
		logger.Warning(err)
	}

	// add app
	if err := Instance().AddNewApp(newApp); err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	retBuf, retErr := json.Marshal(newApp)
	if retErr != nil {
		logger.Warning("create app response json marshal error!")
	}

	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusOK)
	w.Write(retBuf)
}

// for test
func binaryHandler(w http.ResponseWriter, r *http.Request) {
	data := []byte{58, 0, 0, 0, 94, 144, 225, 39, 10, 52, 10, 9, 104, 101, 108, 108, 111, 100, 117, 100, 117, 16, 161, 96, 26, 21, 104, 101, 108, 108, 111, 100, 117, 100, 117, 56, 54, 64, 103, 109, 97, 105, 108, 46, 99, 111, 109, 34, 13, 10, 11, 49, 51, 52, 48, 49, 48, 51, 57, 50, 57, 55}

	Instance().GetWorldSession().HandleMessage(nil, data)
}

func arenaGetPlayerDataHandler(w http.ResponseWriter, r *http.Request) {
	body, err := ioutil.ReadAll(r.Body)
	if err != nil {
		w.Write([]byte(err.Error()))
		return
	}

	var req struct {
		ID int64 `json:"id"`
	}

	if err := json.Unmarshal(body, &req); err != nil {
		w.Write([]byte(err.Error()))
		return
	}

	d, err := Instance().GetGameMgr().GetArena().GetDataByID(req.ID)
	if err != nil {
		w.Write([]byte(err.Error()))
		return
	}

	w.Header().Set("Content-Type", "application/json; charset=utf-8")
	w.WriteHeader(http.StatusOK)
	json.NewEncoder(w).Encode(d)
}

func arenaMatchingListHandler(w http.ResponseWriter, r *http.Request) {
	l := Instance().GetGameMgr().GetArena().GetMatchingList()

	var resp struct {
		ID []int64 `json:"id"`
	}

	resp.ID = l[:]

	w.Header().Set("Content-Type", "application/json; charset=utf-8")
	w.WriteHeader(http.StatusOK)
	json.NewEncoder(w).Encode(resp)
}

func arenaRecordReqListHandler(w http.ResponseWriter, r *http.Request) {
	m := Instance().GetGameMgr().GetArena().GetRecordReqList()

	w.Header().Set("Content-Type", "application/json; charset=utf-8")
	w.WriteHeader(http.StatusOK)
	json.NewEncoder(w).Encode(m)
}

func arenaGetRecordHandler(w http.ResponseWriter, r *http.Request) {
	body, err := ioutil.ReadAll(r.Body)
	if err != nil {
		w.Write([]byte(err.Error()))
		return
	}

	var req struct {
		ID int64 `json:"id"`
	}

	if err := json.Unmarshal(body, &req); err != nil {
		w.Write([]byte(err.Error()))
		return
	}

	d, err := Instance().GetGameMgr().GetArena().GetRecordByID(req.ID)
	if err != nil {
		w.Write([]byte(err.Error()))
		return
	}

	json.NewEncoder(w).Encode(d)
}

func getPlayerInfoHandler(w http.ResponseWriter, r *http.Request) {
	body, err := ioutil.ReadAll(r.Body)
	if err != nil {
		w.Write([]byte(err.Error()))
		return
	}

	var req struct {
		ID int64 `json:"id"`
	}

	if err := json.Unmarshal(body, &req); err != nil {
		w.Write([]byte(err.Error()))
		return
	}

	d := Instance().GetGameMgr().GetPlayerInfoByID(req.ID)
	if d == nil {
		w.Write([]byte(fmt.Sprintf("cannot find player info by id: %d", req.ID)))
		return
	}

	json.NewEncoder(w).Encode(d)
}

func getGuildInfoHandler(w http.ResponseWriter, r *http.Request) {
	body, err := ioutil.ReadAll(r.Body)
	if err != nil {
		w.Write([]byte(err.Error()))
		return
	}

	var req struct {
		ID int64 `json:"id"`
	}

	if err := json.Unmarshal(body, &req); err != nil {
		w.Write([]byte(err.Error()))
		return
	}

	d := Instance().GetGameMgr().GetGuildInfoByID(req.ID)
	if d == nil {
		w.Write([]byte(fmt.Sprintf("cannot find guild info by id: %d", req.ID)))
		return
	}

	json.NewEncoder(w).Encode(d)
}
