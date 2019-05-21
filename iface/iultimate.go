package iface

type IUltimate interface {
	Run()
	Stop()
	WorldMgr() IWorldMgr
	GameMgr() IGameMgr
	Datastore() IDatastore
}
