package rpc_presure

type Options struct {
	RPCTimes int `flag:"times"`
}

func NewOptions() *Options {

	return &Options{
		RPCTimes: 1000,
	}
}
