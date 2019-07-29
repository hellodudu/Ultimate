package logger

import (
	"fmt"
	"os"
	"time"

	"github.com/sirupsen/logrus"
)

// Fields logrus.Fields wrapper
type Fields = logrus.Fields

var (
	logConsole *logrus.Logger
	debug      bool
	console    bool
)

// Init init log system
func Init(d bool, c bool, fn string) {
	debug = d
	console = c
	logConsole = logrus.New()

	// log file name
	t := time.Now()
	fileTime := fmt.Sprintf("%d-%d-%d %d-%d-%d", t.Year(), t.Month(), t.Day(), t.Hour(), t.Minute(), t.Second())
	logFn := fmt.Sprintf("../log/%s_%s.log", fileTime, fn)

	file, err := os.OpenFile(logFn, os.O_CREATE|os.O_WRONLY, 0666)
	if err == nil {
		logrus.SetOutput(file)
	} else {
		Warn("Failed to log to file, using default stderr")
	}

	customFormatter := new(logrus.TextFormatter)
	customFormatter.TimestampFormat = "2006-01-02 15:04:05"
	customFormatter.FullTimestamp = true
	logrus.SetFormatter(customFormatter)
	logConsole.SetFormatter(customFormatter)
}

func Trace(v ...interface{}) {
	if console {
		logConsole.Println(v...)
	}

	if debug {
		logrus.Println(v...)
	}
}

func Info(v ...interface{}) {
	if console {
		logConsole.Info(v...)
	}

	if debug {
		logrus.Info(v...)
	}
}

func Warn(v ...interface{}) {
	if console {
		logConsole.Warn(v...)
	}

	logrus.Warn(v...)
}

func Error(v ...interface{}) {
	if console {
		logConsole.Error(v...)
	}

	logrus.Error(v...)
}

func Fatal(v ...interface{}) {
	if console {
		logConsole.Fatal(v...)
	}

	logrus.Fatal(v...)
}

func Print(v ...interface{}) {
	if console {
		logConsole.Println(v...)
	}

	logrus.Println(v...)
}

func WithFieldsTrace(s string, fields Fields) {
	f := make(logrus.Fields)
	for k, v := range fields {
		f[k] = v
	}

	if console {
		logConsole.WithFields(f).Trace(s)
	}

	logrus.WithFields(f).Trace(s)
}

func WithFieldsDebug(s string, fields Fields) {
	f := make(logrus.Fields)
	for k, v := range fields {
		f[k] = v
	}

	if console {
		logConsole.WithFields(f).Debug(s)
	}

	logrus.WithFields(f).Debug(s)
}

func WithFieldsInfo(s string, fields Fields) {
	f := make(logrus.Fields)
	for k, v := range fields {
		f[k] = v
	}

	if console {
		logConsole.WithFields(f).Info(s)
	}

	logrus.WithFields(f).Info(s)
}

func WithFieldsWarn(s string, fields Fields) {
	f := make(logrus.Fields)
	for k, v := range fields {
		f[k] = v
	}

	if console {
		logConsole.WithFields(f).Warn(s)
	}

	logrus.WithFields(f).Warn(s)
}

func WithFieldsError(s string, fields Fields) {
	f := make(logrus.Fields)
	for k, v := range fields {
		f[k] = v
	}

	if console {
		logConsole.WithFields(f).Error(s)
	}

	logrus.WithFields(f).Error(s)
}
