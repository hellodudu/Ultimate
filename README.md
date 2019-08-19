
# Ultimate server -- A game server written by golang

[![Build Status](https://travis-ci.com/hellodudu/Ultimate.svg?branch=master)](https://travis-ci.com/hellodudu/Ultimate)
[![GoDoc](https://godoc.org/github.com/hellodudu/Ultimate?status.svg)](https://godoc.org/github.com/hellodudu/Ultimate)
[![LICENSE](https://img.shields.io/badge/license-NPL%20(The%20996%20Prohibited%20License)-blue.svg)](https://github.com/996icu/996.ICU/blob/master/LICENSE)
[![Go Report Card](https://goreportcard.com/badge/github.com/hellodudu/Ultimate)](https://goreportcard.com/report/github.com/hellodudu/Ultimate)

## Requirement:

* [go module](https://github.com/golang/go/wiki/Modules)
* [nsq](https://nsq.io) as message broker
* [consul](https://www.consul.io) as service discovery and registry


## Install:

	git clone https://github.com/hellodudu/Ultimate.git

## Run From Path:

* nsq:

        nsqlookupd
        nsqd --lookupd-tcp-address=127.0.0.1:4160

* consul:

        consul agent -dev

* game-service:

        cd game-service
        go run main.go plugin.go --registry=consul --broker=nsq

* arena-service:

        cd arena-service
        go run main.go plugin.go --registry=consul --broker=nsq

## Run From Docker:

* Proto: 

		make proto

* Docker Build:
    
	    make docker


## Run From Docker Hub:

	docker-compose up
