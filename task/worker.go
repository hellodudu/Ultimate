package task

import "fmt"

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

	fmt.Println("work proof with task:", worker.task.Req, ", by worker number:", worker.num)
}
