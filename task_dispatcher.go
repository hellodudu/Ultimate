package task

type TaskDispatcher struct {
	taskChan       chan Task
	workerPoolChan chan WorkerPool
}

func (TaskDispatcher *td) Init() bool {
	td.taskChan = make(chan interface{})
}

func (TaskDispatcher *td) AddTask(task *Task) {
	td.taskChan <- task
}

func (TaskDispatcher *td) Dispatch() {
	go func() {
		for {
			task := <-taskChan
			workerPoolChan <- task
		}
	}()
}
