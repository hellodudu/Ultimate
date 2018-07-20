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
	fmt.Println("work proof with task:", worker.task.Req, ", by worker number:", worker.num)
}
