package ultimate

import (
	"net/http"

	"github.com/hellodudu/Ultimate/task"
)

type Client struct {
	task *task.Task
	w    http.ResponseWriter
	r    *http.Request
}
