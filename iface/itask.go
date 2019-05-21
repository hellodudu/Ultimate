package iface

import "github.com/hellodudu/Ultimate/task"

type IDispatcher interface {
	AddTask(*task.TaskReqInfo)
}
