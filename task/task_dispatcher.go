package task

type TaskDispatcher struct {
	taskChan   chan Task
	workerPool *WorkerPool
}

func (td *TaskDispatcher) Init() bool {
	td.taskChan = make(chan Task)
	td.workerPool = &WorkerPool{}
	td.workerPool.Init(td.taskChan)
	return true
}

func (td *TaskDispatcher) AddTask(task Task) {
	td.taskChan <- task
}
