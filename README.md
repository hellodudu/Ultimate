
# Ultimate server -- A game server written by golang

[![Build Status](https://travis-ci.com/hellodudu/Ultimate.svg?branch=master)](https://travis-ci.com/hellodudu/Ultimate)
[![GoDoc](https://godoc.org/github.com/hellodudu/Ultimate?status.svg)](https://godoc.org/github.com/hellodudu/Ultimate)
[![LICENSE](https://img.shields.io/badge/license-NPL%20(The%20996%20Prohibited%20License)-blue.svg)](https://github.com/996icu/996.ICU/blob/master/LICENSE)


# Install:

	go get github.com/hellodudu/Ultimate


# Makefile:

* Proto: 

		make proto

* Docker Build:
    
	    make build

* Run
    
	    make run


# Run From Docker Hub:

   
	docker run -it -p 7030:7030 -p 8080:8080 hellodudu86/ultimate
