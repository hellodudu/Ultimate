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
)

func Init() bool {
	year, month, day := time.Now().Date()
	hour, min, sec := time.Now().Clock()
	fileTime := fmt.Sprintf("%d-%d-%d %d-%d-%d", year, month, day, hour, min, sec)

	traceName := fmt.Sprintf("log/%s_trace.log", fileTime)
	traceFile, err := os.OpenFile(traceName, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0666)
	if err != nil {
		log.Fatalln("Failed to open log file ", traceName, ":", err)
		return false
	}

	trace = log.New(traceFile, "TRACE: ", log.Ldate|log.Ltime|log.Lshortfile)

	infoName := fmt.Sprintf("log/%s_info.log", fileTime)
	infoFile, err := os.OpenFile(infoName, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0666)
	if err != nil {
		log.Fatalln("Failed to open log file ", infoName, ":", err)
		return false
	}
	info = log.New(infoFile, "INFO: ", log.Ldate|log.Ltime|log.Lshortfile)

	errorName := fmt.Sprintf("log/%s_error.log", fileTime)
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
	trace.Println(color.BlueString(fmt.Sprintln(v...)))
}

func Info(v ...interface{}) {
	info.Println(color.CyanString(fmt.Sprintln(v...)))
}

func Warning(v ...interface{}) {
	warning.Println(color.YellowString(fmt.Sprintln(v...)))
	log.Println(color.YellowString(fmt.Sprintln(v...)))
}

func Error(v ...interface{}) {
	error.Println(color.RedString(fmt.Sprintln(v...)))
	log.Println(color.RedString(fmt.Sprintln(v...)))
}

func Fatal(v ...interface{}) {
	Error(v)
	os.Exit(1)
}
