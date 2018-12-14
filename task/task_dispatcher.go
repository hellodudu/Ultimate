package task

type TaskDispatcher struct {
	taskChan   chan *Task
	workerPool *WorkerPool
}

func NewTaskDispatcher() (*TaskDispatcher, error) {
	td := &TaskDispatcher{
		taskChan:   make(chan *Task, 100),
		workerPool: nil,
	}

	var err error
	if td.workerPool, err = NewWorkerPool(td.taskChan); err != nil {
		return nil, err
	}

	return td, nil
}

func (td *TaskDispatcher) AddTask(request int, callback interface{}) {
	task := &Task{req: request, cb: callback}
	td.taskChan <- task
}
