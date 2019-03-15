package logger

import (
	"fmt"
	"log"
	"os"
	"time"

	"github.com/fatih/color"
)

var (
	trace   *log.Logger
	info    *log.Logger
	warning *log.Logger
	error   *log.Logger
	debug   bool
)

func Init(d bool) bool {
	debug = d
	t := time.Now()
	fileTime := fmt.Sprintf("%d-%d-%d %d-%d-%d", t.Year(), t.Month(), t.Day(), t.Hour(), t.Minute(), t.Second())

	traceName := fmt.Sprintf("log/%s_ultimate_trace.log", fileTime)
	traceFile, err := os.OpenFile(traceName, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0666)
	if err != nil {
		log.Fatalln("Failed to open log file ", traceName, ":", err)
		return false
	}

	trace = log.New(traceFile, "TRACE: ", log.Ldate|log.Ltime|log.Lshortfile)

	infoName := fmt.Sprintf("log/%s_ultimate_info.log", fileTime)
	infoFile, err := os.OpenFile(infoName, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0666)
	if err != nil {
		log.Fatalln("Failed to open log file ", infoName, ":", err)
		return false
	}
	info = log.New(infoFile, "INFO: ", log.Ldate|log.Ltime|log.Lshortfile)

	errorName := fmt.Sprintf("log/%s_ultimate_error.log", fileTime)
	errorFile, err := os.OpenFile(errorName, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0666)
	if err != nil {
		log.Fatalln("Failed to open log file ", errorName, ":", err)
		return false
	}
	error = log.New(errorFile, "ERROR: ", log.Ldate|log.Ltime|log.Lshortfile)
	warning = error

	return true
}

func Trace(v ...interface{}) {
	trace.Println(color.BlueString(fmt.Sprint(v...)))
	if debug {
		log.Println(color.BlueString(fmt.Sprint(v...)))
	}
}

func Info(v ...interface{}) {
	info.Println(color.CyanString(fmt.Sprint(v...)))
	if debug {
		log.Println(color.CyanString(fmt.Sprint(v...)))
	}
}

func Warning(v ...interface{}) {
	warning.Println(color.YellowString(fmt.Sprint(v...)))
	log.Println(color.YellowString(fmt.Sprint(v...)))
}

func Error(v ...interface{}) {
	error.Println(color.RedString(fmt.Sprint(v...)))
	log.Println(color.RedString(fmt.Sprint(v...)))
}

func Fatal(v ...interface{}) {
	Error(v...)
	os.Exit(1)
}

func Print(v ...interface{}) {
	info.Println(color.CyanString(fmt.Sprint(v...)))
	log.Println(color.CyanString(fmt.Sprint(v...)))
}
