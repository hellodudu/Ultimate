package iface

type TaskCallback func(ITCPConn, []byte)

type IDispatcher interface {
	AddTask(ITaskReqInfo)
}

type ITaskReqInfo interface {
	SetID(id int)
	GetID() int
	Call()
}
