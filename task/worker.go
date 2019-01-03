package task

import "log"

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
		log.Println("worker<", worker.num, ">'s task is nil")
	}

	// log.Println("work proof with task:", worker.tasker.GetReq(), ", by worker number:", worker.num)

	worker.tasker.Callback()
}
