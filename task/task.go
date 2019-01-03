package task

type TaskCallback func()

type Tasker interface {
	Callback()
	GetReq() int
}

type Task struct {
	req int // request number
	cb  TaskCallback
}

func (task *Task) GetReq() int {
	return task.req
}

func (task *Task) Callback() {
	task.cb()
}

func NewTask(req int, cb TaskCallback) (*Task, error) {
	return &Task{req: req, cb: cb}, nil
}
