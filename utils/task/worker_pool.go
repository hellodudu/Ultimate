package task

import (
	"context"
	"runtime"

	logger "github.com/hellodudu/Ultimate/utils/log"
)

type workerPool struct {
	taskerChan chan Tasker
	workerChan chan *Worker
	chStop     chan struct{}
	workerList []Worker
	ctx        context.Context
	cancel     context.CancelFunc
}

// NewWorkerPool create new workerpool
func NewWorkerPool(tc chan Tasker) (*workerPool, error) {
	maxWorker := runtime.GOMAXPROCS(runtime.NumCPU())

	pool := &workerPool{
		taskerChan: tc,
		chStop:     make(chan struct{}, 1),
		workerChan: make(chan *Worker, maxWorker),
		workerList: make([]Worker, maxWorker),
	}

	pool.ctx, pool.cancel = context.WithCancel(context.Background())
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
		case <-wp.ctx.Done():
			wp.chStop <- struct{}{}
			return
		case newTasker := <-wp.taskerChan:
			freeWorker := <-wp.workerChan
			freeWorker.AddWork(newTasker)
			freeWorker.Work()
			wp.workerChan <- freeWorker
		}
	}
}

func (wp *workerPool) stop() {
	wp.cancel()
	<-wp.chStop
	close(wp.workerChan)
	close(wp.chStop)
}
