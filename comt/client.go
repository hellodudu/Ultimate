package comt

import (
	"net/http"

	"github.com/hellodudu/comment/task"
)

type Client struct {
	task *task.Task
	w    http.ResponseWriter
	r    *http.Request
}
