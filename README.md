
# Ultimate server -- A game server written by golang

[![Build Status](https://travis-ci.com/hellodudu/Ultimate.svg?branch=master)](https://travis-ci.com/hellodudu/Ultimate)
[![GoDoc](https://godoc.org/github.com/hellodudu/Ultimate?status.svg)](https://godoc.org/github.com/hellodudu/Ultimate)
[![LICENSE](https://img.shields.io/badge/license-NPL%20(The%20996%20Prohibited%20License)-blue.svg)](https://github.com/996icu/996.ICU/blob/master/LICENSE)


# Install:

	go get github.com/hellodudu/Ultimate



# Using tools:

    github.com/go-sql-driver/mysql

# Run From Build:

* Cross Build: 

		env GOOS=linux GOARCH=amd64 CGO_ENABLED=0 go build -v main.go

* Docker Build:
    
	    docker build -t ultimate .

* Docker Run:(on my env)
    
	    docker run -itd -v $GOPATH/src/github.com/hellodudu/Ultimate/config:/config -v $GOPATH/src/github.com/hellodudu/Ultimate/log:/log -p 7030:7030 -p 8088:8080 hellodudu86/ultimate:1.0

# Run From Docker Hub:

   
	docker run -it -p 7030:7030 -p 8080:8080 hellodudu86/ultimate:1.0
