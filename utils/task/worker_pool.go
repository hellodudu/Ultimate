package task

import (
	"runtime"

	"github.com/hellodudu/Ultimate/logger"
)

type workerPool struct {
	taskerChan chan Tasker
	workerChan chan *Worker
	workerList []Worker
}

// NewWorkerPool create new workerpool
func NewWorkerPool(tc chan Tasker) (*workerPool, error) {
	maxWorker := runtime.GOMAXPROCS(runtime.NumCPU())

	pool := &workerPool{
		taskerChan: tc,
		workerChan: make(chan *Worker, maxWorker),
		workerList: make([]Worker, maxWorker),
	}

	logger.Info("init max workers ", maxWorker)

	for n := 1; n <= maxWorker; n++ {
		worker := &Worker{}
		pool.workerList = append(pool.workerList, *worker)
		pool.workerChan <- worker
	}

	go pool.Run()
	return pool, nil
}

// Run workerpool running
func (wp *workerPool) Run() {
	for {
		select {
		case newTasker := <-wp.taskerChan:
			go func(tk Tasker) {
				freeWorker := <-wp.workerChan
				freeWorker.AddWork(tk)
				freeWorker.Work()
				wp.workerChan <- freeWorker
			}(newTasker)
		}
	}
}

func (wp *workerPool) stop() {
	close(wp.taskerChan)
	close(wp.workerChan)
}
