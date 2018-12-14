package task

import (
	"fmt"
	"runtime"
)

type workerPool struct {
	taskChan   chan *Task
	workerChan chan *Worker
	workerList []Worker
}

// NewWorkerPool create new workerpool
func NewWorkerPool(tc chan *Task) (*workerPool, error) {
	maxWorker := runtime.GOMAXPROCS(runtime.NumCPU())

	pool := &workerPool{
		taskChan:   tc,
		workerChan: make(chan *Worker),
		workerList: make([]Worker, maxWorker),
	}

	fmt.Printf("Init max workers %d!\n", maxWorker)

	for n := 1; n <= maxWorker; n++ {
		worker := &Worker{}
		worker.Init(n)
		pool.workerList = append(pool.workerList, *worker)
		go func() {
			pool.workerChan <- worker
		}()
	}

	go pool.Run()
	return pool, nil
}

// Run workerpool running
func (wp *workerPool) Run() {
	for {
		select {
		case newTask := <-wp.taskChan:
			go func(tk *Task) {
				freeWorker := <-wp.workerChan
				freeWorker.AddWork(tk)
				freeWorker.Work()
				wp.workerChan <- freeWorker
			}(newTask)
		}
	}
}
