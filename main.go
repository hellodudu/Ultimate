package main

import (
	"fmt"
	"log"
	"os"
	"os/signal"
	"sort"

	"github.com/fatih/color"
	_ "github.com/go-sql-driver/mysql"
	"github.com/hellodudu/Ultimate/logger"
	ultimate "github.com/hellodudu/Ultimate/server"
)

type TestSort struct {
	key   int
	value int
}

type SortTestSort []*TestSort

func (s SortTestSort) Len() int {
	return len(s)
}

func (s SortTestSort) Swap(a, b int) {
	s[a], s[b] = s[b], s[a]
}

func (s SortTestSort) Less(a, b int) bool {
	return s[a].key < s[b].key
}

func testsort() {
	var mapValue map[int]*TestSort = make(map[int]*TestSort)
	a := &TestSort{key: 10, value: 20}
	b := &TestSort{key: 30, value: 5}
	c := &TestSort{key: 35, value: 93}
	d := &TestSort{key: 81, value: 1}
	e := &TestSort{key: 93, value: 73}
	f := &TestSort{key: 13, value: 83}

	mapValue[a.key] = a
	mapValue[b.key] = b
	mapValue[c.key] = c
	mapValue[d.key] = d
	mapValue[e.key] = e
	mapValue[f.key] = f

	var sortTestSlice SortTestSort = make([]*TestSort, 0)
	sortTestSlice = append(sortTestSlice, a, b, c, d, e, f)

	log.Println(color.MagentaString("before sort: "))
	for k, v := range sortTestSlice {
		log.Println(color.MagentaString("key: ", k, ", value: ", v))
	}
	sort.Sort(sortTestSlice)
	log.Println(color.MagentaString("after sort: "))
	for k, v := range sortTestSlice {
		log.Println(color.MagentaString("key: ", k, ", value: ", v))
	}
}

func main() {
	testsort()
	api, err := ultimate.NewAPI()
	if err != nil {
		logger.Fatal(err)
	}

	api.Run()

	// xmlloader
	// res.NewXmlLoader()

	// server exit
	c := make(chan os.Signal, 1)
	signal.Notify(c, os.Interrupt, os.Kill)
	sig := <-c
	logger.Print(fmt.Sprintf("ultimate server closing down (signal: %v)\n", sig))
	api.Stop()
	os.Exit(0)
}
