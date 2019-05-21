package task

import "net"

type TaskCallback func(net.Conn, []byte)

type TaskReqInfo struct {
	id   int
	con  net.Conn
	data []byte
	cb   TaskCallback
}

type Tasker interface {
	Callback()
	GetReq() int
}

type task struct {
	req TaskReqInfo
}

func (t *task) GetReq() int {
	return t.req.id
}

func (t *task) Callback() {
	t.req.cb(t.req.con, t.req.data)
}

func NewTask(req *TaskReqInfo) Tasker {
	return &task{req: req}
}
