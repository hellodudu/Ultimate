package task

import "github.com/hellodudu/Ultimate/logger"

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
		logger.Info("worker<", worker.num, ">'s task is nil")
	}

	worker.tasker.Callback()
}
