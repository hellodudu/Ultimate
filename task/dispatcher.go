package task

// Dispatcher define
type Dispatcher struct {
	taskerChan chan Tasker
	workerPool *workerPool
}

// NewDispatcher return new dispatcher
func NewDispatcher() (*Dispatcher, error) {
	td := &Dispatcher{
		taskerChan: make(chan Tasker, 100),
		workerPool: nil,
	}

	var err error
	if td.workerPool, err = NewWorkerPool(td.taskerChan); err != nil {
		return nil, err
	}

	return td, nil
}

// AddTask add new task to taskchan
func (td *Dispatcher) AddTask(tasker Tasker) {
	td.taskerChan <- tasker
}
