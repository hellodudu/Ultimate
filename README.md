
# Ultimate server -- A game server written by golang

[![Build Status](https://travis-ci.com/hellodudu/Ultimate.svg?branch=master)](https://travis-ci.com/hellodudu/Ultimate)
[![GoDoc](https://godoc.org/github.com/hellodudu/Ultimate?status.svg)](https://godoc.org/github.com/hellodudu/Ultimate)
[![LICENSE](https://img.shields.io/badge/license-NPL%20(The%20996%20Prohibited%20License)-blue.svg)](https://github.com/996icu/996.ICU/blob/master/LICENSE)

## Requirement:
> go version >= 1.11, using module.

see [go module wiki](https://github.com/golang/go/wiki/Modules)


## Install:

	git clone https://github.com/hellodudu/Ultimate.git

## Run From Path:

* game-service:

        cd game-service
        go run main.go

* arena-service:

        cd arena-service
        go run main.go

## Run From Docker:

* Proto: 

		make proto

* Docker Build:
    
	    make docker

* Run
    
	    make run


## Run From Docker Hub:

	docker-compose up
