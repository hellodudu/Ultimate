package task

import "net"

type TaskCallback func(net.Conn, []byte)

type TaskReqInfo struct {
	ID   int
	Con  net.Conn
	Data []byte
	CB   TaskCallback
}

type Tasker interface {
	Callback()
	GetReq() int
}

type task struct {
	req *TaskReqInfo
}

func (t *task) GetReq() int {
	return t.req.ID
}

func (t *task) Callback() {
	t.req.CB(t.req.Con, t.req.Data)
}

func NewTask(req *TaskReqInfo) Tasker {
	return &task{req: req}
}
