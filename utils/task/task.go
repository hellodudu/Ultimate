package task

import "github.com/hellodudu/Ultimate/iface"

type TaskReqInfo struct {
	ID   int
	Con  iface.ITCPConn
	Data []byte
	CB   iface.TaskCallback
}

func (t *TaskReqInfo) SetID(id int) {
	t.ID = id
}

func (t *TaskReqInfo) GetID() int {
	return t.ID
}

func (t *TaskReqInfo) Call() {
	t.CB(t.Con, t.Data)
}

type Tasker interface {
	Callback()
	GetReq() int
}

type task struct {
	req iface.ITaskReqInfo
}

func (t *task) GetReq() int {
	return t.req.GetID()
}

func (t *task) Callback() {
	t.req.Call()
}

func NewTask(req iface.ITaskReqInfo) Tasker {
	return &task{req: req}
}
