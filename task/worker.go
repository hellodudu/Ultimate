package task

import (
	"fmt"
	"log"
)

type Worker struct {
	tasker Tasker
	num    int
}

func (worker *Worker) Init(num int) {
	worker.num = num
}

func (worker *Worker) AddWork(tk Tasker) {
	worker.tasker = tk
}

func (worker *Worker) Work() {
	if worker.tasker.GetReq() == 0 {
		fmt.Println("worker<", worker.num, ">'s task is nil")
	}

	log.Println("work proof with task:", worker.tasker.GetReq(), ", by worker number:", worker.num)

	worker.tasker.Callback(worker.tasker)

	// fun := reflect.ValueOf(worker.task.cb)
	// param := make([]reflect.Value, 1)
	// param[0] = reflect.ValueOf(worker.task)
	// fun.Call(param)
}
