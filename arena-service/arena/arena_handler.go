package arena

import (
	"context"
	"encoding/json"

	"github.com/hellodudu/Ultimate/logger"
	pbArena "github.com/hellodudu/Ultimate/proto/arena"
	pbGame "github.com/hellodudu/Ultimate/proto/game"
)

// RPCHandler rpc handler
type RPCHandler struct {
	ctx     context.Context
	arena   *Arena
	gameCli pbGame.GameServiceClient
}

/////////////////////////////////////////////
// rpc call
/////////////////////////////////////////////
func (h *RPCHandler) GetPlayerInfoByID(id int64) (*pbGame.GetPlayerInfoByIDReply, error) {
	req := &pbGame.GetPlayerInfoByIDRequest{Id: id}
	return h.gameCli.GetPlayerInfoByID(h.ctx, req)
}

/////////////////////////////////////////////
// rpc receive
/////////////////////////////////////////////
func (h *RPCHandler) GetSeasonData(ctx context.Context, req *pbArena.GetSeasonDataRequest, rsp *pbArena.GetSeasonDataReply) error {
	rsp.Season = int32(h.arena.season())
	rsp.SeasonEndTime = int32(h.arena.seasonEndTime())
	return nil
}

func (h *RPCHandler) GetChampion(ctx context.Context, req *pbArena.GetChampionRequest, rsp *pbArena.GetChampionReply) error {
	rsp.Data = h.arena.getChampion()
	return nil
}

func (h *RPCHandler) AddRecord(ctx context.Context, req *pbArena.AddRecordRequest, rsp *pbArena.AddRecordReply) error {
	h.arena.addRecord(req.Data)
	return nil
}

func (h *RPCHandler) BattleResult(ctx context.Context, req *pbArena.BattleResultRequest, rsp *pbArena.BattleResultReply) error {
	logger.Info("Received ArenaService.BattleResult request")
	h.arena.battleResult(req.AttackId, req.TargetId, req.AttackWin)
	return nil
}

func (h *RPCHandler) GetRank(ctx context.Context, req *pbArena.GetRankRequest, rsp *pbArena.GetRankReply) error {
	logger.Info("Received ArenaService.GetRank request")
	h.arena.requestRank(req.PlayerId, req.Page)
	return nil
}

func (h *RPCHandler) GetArenaDataNum(ctx context.Context, req *pbArena.GetArenaDataNumRequest, rsp *pbArena.GetArenaDataNumReply) error {
	logger.Info("Received ArenaService.GetArenaDataNum request")
	h.arena.getArenaDataNum()
	return nil
}

func (h *RPCHandler) GetRecordNum(ctx context.Context, req *pbArena.GetRecordNumRequest, rsp *pbArena.GetRecordNumReply) error {
	logger.Info("Received ArenaService.GetRecordNum request")
	rsp.Num = int32(h.arena.getRecordNum())
	return nil
}

func (h *RPCHandler) GetMatchingList(ctx context.Context, req *pbArena.GetMatchingListRequest, rsp *pbArena.GetMatchingListReply) error {
	logger.Info("Received ArenaService.GetMatchingList request")
	rsp.Ids = h.arena.getMatchingList()
	return nil
}

func (h *RPCHandler) GetRecordReqList(ctx context.Context, req *pbArena.GetRecordReqListRequest, rsp *pbArena.GetRecordReqListReply) error {
	logger.Info("Received ArenaService.GetRecordReqList request")
	m := h.arena.GetRecordReqList()
	for k, v := range m {
		rsp.ReqList = append(rsp.ReqList, &pbArena.RecordReqList{PlayerId: k, Time: int32(v)})
	}
	return nil
}

func (h *RPCHandler) GetRecordByID(ctx context.Context, req *pbArena.GetRecordByIDRequest, rsp *pbArena.GetRecordByIDReply) error {
	logger.Info("Received ArenaService.GetRecordByID request")
	var err error
	rsp.Record, err = h.arena.getRecordByID(req.Id)
	return err
}

func (h *RPCHandler) GetRankListByPage(ctx context.Context, req *pbArena.GetRankListByPageRequest, rsp *pbArena.GetRankListByPageReply) error {
	logger.Info("Received ArenaService.GetRankListByPage request")
	data := h.arena.getRankListByPage(int(req.Page))

	var err error
	rsp.Data, err = json.Marshal(data)
	return err
}

func (h *RPCHandler) SaveChampion(ctx context.Context, req *pbArena.SaveChampionRequest, rsp *pbArena.SaveChampionReply) error {
	logger.Info("Received ArenaService.SaveChampion request")
	h.arena.saveChampion()
	return nil
}

func (h *RPCHandler) WeekEnd(ctx context.Context, req *pbArena.WeekEndRequest, rsp *pbArena.WeekEndReply) error {
	logger.Info("Received ArenaService.WeekEnd request")
	h.arena.weekEnd()
	return nil
}
