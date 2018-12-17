package task

import (
	"fmt"
	"log"
)

type Worker struct {
	tasker *Tasker
	num    int
}

func (worker *Worker) Init(num int) {
	worker.num = num
}

func (worker *Worker) AddWork(tk *Tasker) {
	worker.tasker = tk
}

func (worker *Worker) Work() {
	if worker.tasker == nil {
		fmt.Println("worker<", worker.num, ">'s task is nil")
	}

	log.Println("work proof with task:", worker.task.req, ", by worker number:", worker.num)

	worker.task.cb(worker.task)

	// fun := reflect.ValueOf(worker.task.cb)
	// param := make([]reflect.Value, 1)
	// param[0] = reflect.ValueOf(worker.task)
	// fun.Call(param)
}
