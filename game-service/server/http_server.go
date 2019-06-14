package server

import (
	"context"
	"encoding/json"
	"expvar"
	"fmt"
	"io/ioutil"
	"net/http"
	_ "net/http/pprof"
	"runtime"
	"time"

	"github.com/hellodudu/Ultimate/iface"
	"github.com/hellodudu/Ultimate/logger"
	pbArena "github.com/hellodudu/Ultimate/proto/arena"
	"github.com/hellodudu/Ultimate/utils/global"
	"github.com/sirupsen/logrus"
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

func (s *HttpServer) getArenaPlayerDataNum() interface{} {
	return s.gm.GetArenaDataNum()
}

func (s *HttpServer) getArenaRecordNum() interface{} {
	return s.gm.GetArenaRecordNum()
}

type HttpServer struct {
	ctx      context.Context
	cancel   context.CancelFunc
	gm       iface.IGameMgr
	arenaCli pbArena.ArenaServiceClient
}

func NewHttpServer(gm iface.IGameMgr) *HttpServer {
	s := &HttpServer{
		gm:       gm,
		arenaCli: pbArena.NewArenaServiceClient("", nil),
	}

	s.ctx, s.cancel = context.WithCancel(context.Background())
	return s
}

func (s *HttpServer) Run() {

	expvar.Publish("ticktime", expvar.Func(calculateUptime))
	expvar.Publish("version", expvar.Func(currentGoVersion))
	expvar.Publish("cores", expvar.Func(getNumCPUs))
	expvar.Publish("os", expvar.Func(getGoOS))
	expvar.Publish("goroutine", expvar.Func(getNumGoroutins))
	expvar.Publish("gcpause", expvar.Func(getLastGCPauseTime))
	expvar.Publish("arena_player_data_num", expvar.Func(s.getArenaPlayerDataNum))
	expvar.Publish("arena_record_num", expvar.Func(s.getArenaRecordNum))

	http.HandleFunc("/arena_matching_list", s.arenaMatchingListHandler)
	http.HandleFunc("/arena_record_req_list", s.arenaRecordReqListHandler)
	http.HandleFunc("/arena_get_record", s.arenaGetRecordHandler)
	http.HandleFunc("/arena_rank_list", s.arenaGetRankListHandler)
	http.HandleFunc("/arena_save_champion", s.arenaSaveChampion)
	http.HandleFunc("/arena_weekend", s.arenaWeekEnd)
	http.HandleFunc("/player_info", s.getPlayerInfoHandler)
	http.HandleFunc("/guild_info", s.getGuildInfoHandler)

	addr, err := global.IniMgr.GetIniValue("config/ultimate.ini", "listen", "HttpListenAddr")
	if err != nil {
		logger.Error("cannot read ini HttpListenAddr!")
		return
	}

	logger.Error(http.ListenAndServe(addr, nil))

}

func (s *HttpServer) arenaMatchingListHandler(w http.ResponseWriter, r *http.Request) {
	req := &pbArena.GetMatchingListRequest{}
	rsp, err := s.arenaCli.GetMatchingList(s.ctx, req)
	if err != nil {
		logger.WithFieldsWarn("GetMatchingList Response", logrus.Fields{
			"error": err,
		})
		return
	}

	var resp struct {
		ID []int64 `json:"id"`
	}

	resp.ID = rsp.Ids

	w.Header().Set("Content-Type", "application/json; charset=utf-8")
	w.WriteHeader(http.StatusOK)
	json.NewEncoder(w).Encode(resp)
}

func (s *HttpServer) arenaRecordReqListHandler(w http.ResponseWriter, r *http.Request) {
	req := &pbArena.GetRecordReqListRequest{}
	rsp, err := s.arenaCli.GetRecordReqList(s.ctx, req)
	if err != nil {
		logger.WithFieldsWarn("GetRecordReqList Response", logrus.Fields{
			"error": err,
		})
		return
	}

	w.Header().Set("Content-Type", "application/json; charset=utf-8")
	w.WriteHeader(http.StatusOK)
	json.NewEncoder(w).Encode(rsp.ReqList)
}

func (s *HttpServer) arenaGetRecordHandler(w http.ResponseWriter, r *http.Request) {
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

	rpcReq := &pbArena.GetRecordByIDRequest{Id: req.ID}
	rsp, err := s.arenaCli.GetRecordByID(s.ctx, rpcReq)
	if err != nil {
		logger.WithFieldsWarn("GetRecordByID Response", logrus.Fields{
			"error": err,
		})
		w.Write([]byte(err.Error()))
		return
	}

	json.NewEncoder(w).Encode(rsp.Record)
}

func (s *HttpServer) arenaGetRankListHandler(w http.ResponseWriter, r *http.Request) {
	body, err := ioutil.ReadAll(r.Body)
	if err != nil {
		w.Write([]byte(err.Error()))
		return
	}

	var req struct {
		Page int `json:"page"`
	}

	if err := json.Unmarshal(body, &req); err != nil {
		w.Write([]byte(err.Error()))
		return
	}

	d := s.gm.Arena().GetRankListByPage(req.Page)
	json.NewEncoder(w).Encode(d)
}

func (s *HttpServer) arenaSaveChampion(w http.ResponseWriter, r *http.Request) {
	s.gm.Arena().SaveChampion()
}

func (s *HttpServer) arenaWeekEnd(w http.ResponseWriter, r *http.Request) {
	s.gm.Arena().WeekEnd()
}

func (s *HttpServer) getPlayerInfoHandler(w http.ResponseWriter, r *http.Request) {
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

	d := s.gm.GetPlayerInfoByID(req.ID)
	if d == nil {
		w.Write([]byte(fmt.Sprintf("cannot find player info by id: %d", req.ID)))
		return
	}

	json.NewEncoder(w).Encode(d)
}

func (s *HttpServer) getGuildInfoHandler(w http.ResponseWriter, r *http.Request) {
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

	d := s.gm.GetGuildInfoByID(req.ID)
	if d == nil {
		w.Write([]byte(fmt.Sprintf("cannot find guild info by id: %d", req.ID)))
		return
	}

	json.NewEncoder(w).Encode(d)
}
