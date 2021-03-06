package task

import (
	"sync"

	"github.com/hellodudu/Ultimate/iface"
	log "github.com/rs/zerolog/log"
)

// Dispatcher define
type Dispatcher struct {
	mu         sync.Mutex
	reqID      int
	closed     bool
	taskerChan chan Tasker
	workerPool *workerPool
}

// NewDispatcher return new dispatcher
func NewDispatcher() (*Dispatcher, error) {
	td := &Dispatcher{
		reqID:      0,
		closed:     false,
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
func (td *Dispatcher) AddTask(req iface.ITaskReqInfo) {
	if td.closed {
		taskInfo := req.(*TaskReqInfo)
		log.Info().
			Int("id", taskInfo.ID).
			Interface("con", taskInfo.Con).
			Interface("cb", taskInfo.CB).
			Bytes("data", taskInfo.Data).
			Msg("AddTask after dispatcher is closed")
		return
	}

	req.SetID(td.genReqID())
	td.taskerChan <- NewTask(req)
}

// Stop graceful close channel
func (td *Dispatcher) Stop() {
	td.mu.Lock()
	td.closed = true
	td.mu.Unlock()

	td.workerPool.stop()
	close(td.taskerChan)
}
