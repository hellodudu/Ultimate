package task

import (
	"fmt"
	"log"
	"reflect"
)

type Worker struct {
	task *Task
	num  int
}

func (worker *Worker) Init(num int) {
	worker.num = num
}

func (worker *Worker) AddWork(tk *Task) {
	worker.task = tk
}

func (worker *Worker) Work() {
	if worker.task == nil {
		fmt.Println("worker<", worker.num, ">'s task is nil")
	}

	log.Println("work proof with task:", worker.task.req, ", by worker number:", worker.num)

	// worker.task.cb(worker.task)

	fun := reflect.ValueOf(worker.task.cb)
	param := make([]reflect.Value, 1)
	param[0] = reflect.ValueOf(worker.task)
	fun.Call(param)
}
