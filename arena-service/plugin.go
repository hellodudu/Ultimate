package main

import (
	_ "github.com/micro/go-plugins/broker/nsq"
	_ "github.com/micro/go-plugins/transport/tcp"

	_ "github.com/micro/go-plugins/client/grpc"
)
