package task

import (
	"runtime"
)

type WorkerPool struct {
	taskChan   chan Task
	workerChan chan *Worker
	workerList []Worker
}

func (wp *WorkerPool) Init(tc chan Task) bool {
	wp.taskChan = tc
	wp.workerChan = make(chan *Worker)

	maxWorker := runtime.GOMAXPROCS(runtime.NumCPU())
	wp.workerList = make([]Worker, maxWorker)

	for n := 0; n < maxWorker; n++ {
		worker := &Worker{}
		worker.Init(n)
		wp.workerList = append(wp.workerList, *worker)
		go func() {
			wp.workerChan <- worker
		}()
	}

	go wp.Run()
	return true
}

func (wp *WorkerPool) Run() {
	for {
		select {
		case newTask := <-wp.taskChan:
			go func(tk *Task) {
				freeWorker := <-wp.workerChan
				freeWorker.AddWork(tk)
				freeWorker.Work()
				wp.workerChan <- freeWorker
			}(&newTask)
		}
	}
}
