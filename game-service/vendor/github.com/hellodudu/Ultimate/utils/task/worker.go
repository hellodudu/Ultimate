package task

type Worker struct {
	tasker Tasker
}

func (worker *Worker) AddWork(tk Tasker) {
	worker.tasker = tk
}

func (worker *Worker) Work() {
	worker.tasker.Callback()
}
