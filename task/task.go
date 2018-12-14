package task

type Task struct {
	req int         // request number
	cb  interface{} // callback
}

func (task *Task) GetReq() int {
	return task.req
}

func (task *Task) GetCallback() interface{} {
	return task.cb
}
