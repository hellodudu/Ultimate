package logger

import (
	"fmt"
	"os"
	"time"

	"github.com/sirupsen/logrus"
)

var (
	logConsole *logrus.Logger
	debug      bool
)

func Init(d bool, fn string) {
	debug = d
	logConsole = logrus.New()
	logConsole.SetReportCaller(true)

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

	logrus.SetReportCaller(true)
}

func Trace(v ...interface{}) {
	logConsole.Println(v...)
	if debug {
		logrus.Println(v...)
	}
}

func Info(v ...interface{}) {
	logConsole.Info(v...)
	if debug {
		logrus.Info(v...)
	}
}

func Warn(v ...interface{}) {
	logConsole.Warn(v...)
	logrus.Warn(v...)
}

func Error(v ...interface{}) {
	logConsole.Error(v...)
	logrus.Error(v...)
}

func Fatal(v ...interface{}) {
	logrus.Fatal(v...)
}

func Print(v ...interface{}) {
	logConsole.Println(v...)
	logrus.Println(v...)
}

func WithFieldsInfo(s string, fields logrus.Fields) {
	logConsole.WithFields(fields).Info(s)
	logrus.WithFields(fields).Info(s)
}

func WithFieldsWarn(s string, fields logrus.Fields) {
	logConsole.WithFields(fields).Warn(s)
	logrus.WithFields(fields).Warn(s)
}

func WithFieldsError(s string, fields logrus.Fields) {
	logConsole.WithFields(fields).Error(s)
	logrus.WithFields(fields).Error(s)
}
