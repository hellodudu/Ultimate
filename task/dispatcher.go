package task

// Dispatcher define
type Dispatcher struct {
	taskChan   chan *Task
	workerPool *workerPool
}

// NewDispatcher return new dispatcher
func NewDispatcher() (*Dispatcher, error) {
	td := &Dispatcher{
		taskChan:   make(chan *Task, 100),
		workerPool: nil,
	}

	var err error
	if td.workerPool, err = NewWorkerPool(td.taskChan); err != nil {
		return nil, err
	}

	return td, nil
}

// AddTask add new task to taskchan
func (td *Dispatcher) AddTask(request int, callback interface{}) {
	task := &Task{req: request, cb: callback}
	td.taskChan <- task
}
