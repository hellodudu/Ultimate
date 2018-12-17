package task

import "net/http"

type TaskCallback func(http.ResponseWriter, *http.Request, *Task)

type Tasker interface {
	Write([]byte) (int, error)
	Callback(*Task)
	GetReq() int
}

type Task struct {
	req int // request number
}

type HttpTask struct {
	tk Task
	w  http.ResponseWriter
	r  *http.Request
	cb TaskCallback
}

func (task *Task) GetReq() int {
	return task.req
}

func (task Task) Write([]byte) (int, error) {
	return 0, nil
}

func (task Task) Callback() {

}

func (task *HttpTask) Write(b []byte) (int, error) {
	task.rw.Write(b)
}

func (task *HttpTask) Callback(tk *Task) {
	task.cb(task.rw, task.rq, tk)
}
