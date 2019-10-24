package rpc_presure

type Options struct {
	RPCTimes int `flag:"times"`
	Workers  int `flag:"workers"`
}

func NewOptions() *Options {

	return &Options{
		RPCTimes: 100,
		Workers:  10,
	}
}
