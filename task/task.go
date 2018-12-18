package task

import (
	"log"
	"net/http"
	"reflect"
)

type TaskCallback func(Tasker)

type Tasker interface {
	Write([]byte) (int, error)
	Callback(Tasker)
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

func (task *Task) Write([]byte) (int, error) {
	return 0, nil
}

func (task Task) Callback(Tasker) {

}

func (task *HttpTask) GetReq() int {
	return task.tk.req
}

func (task *HttpTask) Write(b []byte) (int, error) {
	log.Printf("http task write %s in %+v\n", b, reflect.TypeOf(task.w))
	return task.w.Write(b)
}

func (task *HttpTask) Callback(tk Tasker) {
	task.cb(task)
}

func NewTask(req int) (*Task, error) {
	return &Task{req: req}, nil
}

func NewHttpTask(req int, w http.ResponseWriter, r *http.Request, cb TaskCallback) (*HttpTask, error) {
	return &HttpTask{tk: Task{req}, w: w, r: r, cb: cb}, nil
}
