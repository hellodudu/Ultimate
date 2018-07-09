package task

import "fmt"

type Worker struct {
	task *Task
	num  int
}

type WorkerPool struct {
	taskChan chan Task
}

func (Worker *worker) Init(num int) {
	worker.num = num
}

func (Worker *worker) AddWork(tk *Task) {
	worker.task = tk
}

func (Worker *worker) Work() {
	fmt.Println("work proof with task:", worker.task.req, ", by worker number:", worker.num)
}
