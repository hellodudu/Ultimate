package utils

import (
	"sync"

	fun "github.com/hellodudu/Ultimate/utils"
)

type WaitGroupWrapper struct {
	sync.WaitGroup
}

func (w *WaitGroupWrapper) Wrap(cb func()) {
	w.Add(1)
	go func() {
		defer func() {
			fun.CaptureException()
			w.Done()
		}()
		cb()
	}()
}
