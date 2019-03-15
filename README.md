
# Ultimate server -- A game server writed by golang

[![Build Status](https://travis-ci.com/hellodudu/Ultimate.svg?branch=master)](https://travis-ci.com/hellodudu/Ultimate)
[![GoDoc](https://godoc.org/github.com/hellodudu/Ultimate?status.svg)](https://godoc.org/github.com/hellodudu/Ultimate)

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
    
	    docker run -itd -v $GOPATH/src/github.com/hellodudu/Ultimate/config:/config -v $GOPATH/src/github.com/hellodudu/Ultimate/log:/log -p 7030:7030 -p 8088:8080 ultimate

# Run From Docker Hub:

   
	docker run -it -p 7030:7030 -p 8080:8080 hellodudu86/ultimate:1.0
