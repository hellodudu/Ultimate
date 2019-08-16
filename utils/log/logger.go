package logger

import (
	"fmt"
	"os"
	"time"

	"github.com/go-log/log"
	"github.com/sirupsen/logrus"
)

// Fields logrus.Fields wrapper
type Fields = logrus.Fields

type Logger struct {
	console     *logrus.Logger
	file        *logrus.Logger
	debug       bool
	showConsole bool
}

func (l *Logger) Log(v ...interface{}) {
	Print(v...)
}

func (l *Logger) Logf(format string, v ...interface{}) {
	Printf(format, v...)
}

var (
	defaultLogger = &Logger{}
)

// Init init log system
func Init(d bool, c bool, fn string) {
	defaultLogger.debug = d
	defaultLogger.showConsole = c
	defaultLogger.console = logrus.New()
	defaultLogger.file = logrus.New()

	// log file name
	t := time.Now()
	fileTime := fmt.Sprintf("%d-%d-%d %d-%d-%d", t.Year(), t.Month(), t.Day(), t.Hour(), t.Minute(), t.Second())
	logFn := fmt.Sprintf("../log/%s_%s.log", fileTime, fn)

	file, err := os.OpenFile(logFn, os.O_CREATE|os.O_WRONLY, 0666)
	if err == nil {
		defaultLogger.file.SetOutput(file)
	} else {
		Warn("Failed to log to file, using default stderr")
	}

	fileFormatter := new(logrus.TextFormatter)
	fileFormatter.TimestampFormat = "2006-01-02 15:04:05"
	fileFormatter.FullTimestamp = true
	fileFormatter.DisableColors = true
	defaultLogger.file.SetFormatter(fileFormatter)

	consoleFormatter := new(logrus.TextFormatter)
	consoleFormatter.TimestampFormat = "2006-01-02 15:04:05"
	consoleFormatter.FullTimestamp = true
	consoleFormatter.ForceColors = true
	defaultLogger.console.SetFormatter(consoleFormatter)
}

func DefaultLogger() log.Logger {
	return defaultLogger
}

func Trace(v ...interface{}) {
	if defaultLogger.showConsole {
		defaultLogger.console.Println(v...)
	}

	if defaultLogger.debug {
		defaultLogger.file.Println(v...)
	}
}

func Info(v ...interface{}) {
	if defaultLogger.showConsole {
		defaultLogger.console.Info(v...)
	}

	if defaultLogger.debug {
		defaultLogger.file.Info(v...)
	}
}

func Warn(v ...interface{}) {
	if defaultLogger.showConsole {
		defaultLogger.console.Warn(v...)
	}

	defaultLogger.file.Warn(v...)
}

func Error(v ...interface{}) {
	if defaultLogger.showConsole {
		defaultLogger.console.Error(v...)
	}

	defaultLogger.file.Error(v...)
}

func Fatal(v ...interface{}) {
	if defaultLogger.showConsole {
		defaultLogger.console.Fatal(v...)
	}

	defaultLogger.file.Fatal(v...)
}

func Print(v ...interface{}) {
	if defaultLogger.showConsole {
		defaultLogger.console.Info(v...)
	}

	defaultLogger.file.Info(v...)
}

func Printf(format string, v ...interface{}) {
	if defaultLogger.showConsole {
		defaultLogger.console.Info(fmt.Sprintf(format, v...))
	}

	defaultLogger.file.Info(fmt.Sprintf(format, v...))
}

func WithFieldsTrace(s string, fields Fields) {
	f := make(logrus.Fields)
	for k, v := range fields {
		f[k] = v
	}

	if defaultLogger.showConsole {
		defaultLogger.console.WithFields(f).Trace(s)
	}

	defaultLogger.file.WithFields(f).Trace(s)
}

func WithFieldsDebug(s string, fields Fields) {
	f := make(logrus.Fields)
	for k, v := range fields {
		f[k] = v
	}

	if defaultLogger.showConsole {
		defaultLogger.console.WithFields(f).Debug(s)
	}

	defaultLogger.file.WithFields(f).Debug(s)
}

func WithFieldsInfo(s string, fields Fields) {
	f := make(logrus.Fields)
	for k, v := range fields {
		f[k] = v
	}

	if defaultLogger.showConsole {
		defaultLogger.console.WithFields(f).Info(s)
	}

	defaultLogger.file.WithFields(f).Info(s)
}

func WithFieldsWarn(s string, fields Fields) {
	f := make(logrus.Fields)
	for k, v := range fields {
		f[k] = v
	}

	if defaultLogger.showConsole {
		defaultLogger.console.WithFields(f).Warn(s)
	}

	defaultLogger.file.WithFields(f).Warn(s)
}

func WithFieldsError(s string, fields Fields) {
	f := make(logrus.Fields)
	for k, v := range fields {
		f[k] = v
	}

	if defaultLogger.showConsole {
		defaultLogger.console.WithFields(f).Error(s)
	}

	defaultLogger.file.WithFields(f).Error(s)
}
