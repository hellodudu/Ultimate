package task

import (
	"sync"
)

// Dispatcher define
type Dispatcher struct {
	mu         sync.Mutex
	reqID      int
	taskerChan chan Tasker
	workerPool *workerPool
}

// NewDispatcher return new dispatcher
func NewDispatcher() (*Dispatcher, error) {
	td := &Dispatcher{
		reqID:      0,
		taskerChan: make(chan Tasker, 100),
		workerPool: nil,
	}

	var err error
	if td.workerPool, err = NewWorkerPool(td.taskerChan); err != nil {
		return nil, err
	}

	return td, nil
}

func (td *Dispatcher) genReqID() int {
	defer td.mu.Unlock()
	td.mu.Lock()
	td.reqID++
	return td.reqID
}

// AddTask add new task to taskchan
func (td *Dispatcher) AddTask(req *TaskReqInfo) {
	req.id = td.genReq()
	td.taskerChan <- NewTask(req)
}
