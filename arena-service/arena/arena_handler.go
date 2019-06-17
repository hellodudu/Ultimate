package arena

import (
	"context"
	"encoding/json"

	"github.com/hellodudu/Ultimate/logger"
	pbArena "github.com/hellodudu/Ultimate/proto/arena"
	pbGame "github.com/hellodudu/Ultimate/proto/game"
)

type ArenaHandler struct {
	ctx     context.Context
	arena   *Arena
	gameCli pbGame.GameServiceClient
}

func NewArenaHandler(ctx context.Context, arena *Arena) *ArenaHandler {
	h := &ArenaHandler{
		ctx:     ctx,
		arena:   arena,
		gameCli: pbGame.NewGameServiceClient("", nil),
	}

	return h
}

/////////////////////////////////////////////
// rpc call
/////////////////////////////////////////////

func (h *ArenaHandler) GetPlayerInfoByID(id int64) (*pbGame.GetPlayerInfoByIDReply, error) {
	req := &pbGame.GetPlayerInfoByIDRequest{Id: id}
	return h.gameCli.GetPlayerInfoByID(h.ctx, req)
}

func (h *ArenaHandler) SendWorldMessage(id uint32, name string, data []byte) (*pbGame.SendWorldMessageReply, error) {
	req := &pbGame.SendWorldMessageRequest{
		Id:      id,
		MsgName: name,
		MsgData: data,
	}

	return h.gameCli.SendWorldMessage(h.ctx, req)
}

func (h *ArenaHandler) BroadCast(name string, data []byte) (*pbGame.BroadCastReply, error) {
	req := &pbGame.BroadCastRequest{
		MsgName: name,
		MsgData: data,
	}

	return h.gameCli.BroadCast(h.ctx, req)
}

/////////////////////////////////////////////
// rpc receive
/////////////////////////////////////////////
func (h *ArenaHandler) GetSeasonData(ctx context.Context, req *pbArena.GetSeasonDataRequest, rsp *pbArena.GetSeasonDataReply) error {
	logger.Info("Received ArenaSevice.GetSeasonData request")
	rsp.Season = int32(h.arena.season())
	rsp.SeasonEndTime = int32(h.arena.seasonEndTime())
	return nil
}

func (h *ArenaHandler) GetChampion(ctx context.Context, req *pbArena.GetChampionRequest, rsp *pbArena.GetChampionReply) error {
	logger.Info("Received ArenaSevice.GetChampion request")
	rsp.Data = h.arena.getChampion()
	return nil
}

func (h *ArenaHandler) Matching(ctx context.Context, req *pbArena.MatchingRequest, rsp *pbArena.MatchingReply) error {
	logger.Info("Received ArenaService.Matching request")
	h.arena.matching(req.Id)
	return nil
}

func (h *ArenaHandler) AddRecord(ctx context.Context, req *pbArena.AddRecordRequest, rsp *pbArena.AddRecordReply) error {
	logger.Info("Received ArenaService.AddRecord request")
	h.arena.addRecord(req.Data)
	return nil
}

func (h *ArenaHandler) BattleResult(ctx context.Context, req *pbArena.BattleResultRequest, rsp *pbArena.BattleResultReply) error {
	logger.Info("Received ArenaService.BattleResult request")
	h.arena.battleResult(req.AttackId, req.TargetId, req.AttackWin)
	return nil
}

func (h *ArenaHandler) GetRank(ctx context.Context, req *pbArena.GetRankRequest, rsp *pbArena.GetRankReply) error {
	logger.Info("Received ArenaService.GetRank request")
	h.arena.requestRank(req.PlayerId, req.Page)
	return nil
}

func (h *ArenaHandler) GetArenaDataNum(ctx context.Context, req *pbArena.GetArenaDataNumRequest, rsp *pbArena.GetArenaDataNumReply) error {
	logger.Info("Received ArenaService.GetArenaDataNum request")
	h.arena.getArenaDataNum()
	return nil
}

func (h *ArenaHandler) GetRecordNum(ctx context.Context, req *pbArena.GetRecordNumRequest, rsp *pbArena.GetRecordNumReply) error {
	logger.Info("Received ArenaService.GetRecordNum request")
	rsp.Num = int32(h.arena.getRecordNum())
	return nil
}

func (h *ArenaHandler) GetMatchingList(ctx context.Context, req *pbArena.GetMatchingListRequest, rsp *pbArena.GetMatchingListReply) error {
	logger.Info("Received ArenaService.GetMatchingList request")
	rsp.Ids = h.arena.getMatchingList()
	return nil
}

func (h *ArenaHandler) GetRecordReqList(ctx context.Context, req *pbArena.GetRecordReqListRequest, rsp *pbArena.GetRecordReqListReply) error {
	logger.Info("Received ArenaService.GetRecordReqList request")
	m := h.arena.GetRecordReqList()
	for k, v := range m {
		rsp.ReqList = append(rsp.ReqList, &pbArena.RecordReqList{PlayerId: k, Time: int32(v)})
	}
	return nil
}

func (h *ArenaHandler) GetRecordByID(ctx context.Context, req *pbArena.GetRecordByIDRequest, rsp *pbArena.GetRecordByIDReply) error {
	logger.Info("Received ArenaService.GetRecordByID request")
	var err error
	rsp.Record, err = h.arena.getRecordByID(req.Id)
	return err
}

func (h *ArenaHandler) GetRankListByPage(ctx context.Context, req *pbArena.GetRankListByPageRequest, rsp *pbArena.GetRankListByPageReply) error {
	logger.Info("Received ArenaService.GetRankListByPage request")
	data := h.arena.getRankListByPage(int(req.Page))

	var err error
	rsp.Data, err = json.Marshal(data)
	return err
}

func (h *ArenaHandler) SaveChampion(ctx context.Context, req *pbArena.SaveChampionRequest, rsp *pbArena.SaveChampionReply) error {
	logger.Info("Received ArenaService.SaveChampion request")
	h.arena.saveChampion()
	return nil
}

func (h *ArenaHandler) WeekEnd(ctx context.Context, req *pbArena.WeekEndRequest, rsp *pbArena.WeekEndReply) error {
	logger.Info("Received ArenaService.WeekEnd request")
	h.arena.weekEnd()
	return nil
}

/////////////////////////////////////
// subscribe
/////////////////////////////////////
// func (arena *Arena) SubHandler(ctx context.Context, msg *pbArena.Message) error {
// 	logger.Info("Function Received message: ", msg.Say)
// 	return nil
// }
