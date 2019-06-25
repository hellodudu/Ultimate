package task

import (
	"testing"

	"github.com/hellodudu/Ultimate/logger"
)

func init() {
	logger.Init(false, false, "dispatcher_test")
}

func TestNewDispatcher(t *testing.T) {

	td, err := NewDispatcher()
	if err != nil {
		t.Error("NewDispatcher failed")
	}

	id := td.genReqID()
	if id != 1 {
		t.Error("genReqID failed")
	}
}
