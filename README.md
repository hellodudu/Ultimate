[![Build Status](https://travis-ci.com/hellodudu/Ultimate.svg?branch=master)](https://travis-ci.com/hellodudu/Ultimate)
[![GoDoc](https://godoc.org/github.com/hellodudu/Ultimate?status.svg)](https://godoc.org/github.com/hellodudu/Ultimate)
[![LICENSE](https://img.shields.io/badge/license-NPL%20(The%20996%20Prohibited%20License)-blue.svg)](https://github.com/996icu/996.ICU/blob/master/LICENSE)
[![Go Report Card](https://goreportcard.com/badge/github.com/hellodudu/Ultimate)](https://goreportcard.com/report/github.com/hellodudu/Ultimate)

# Ultimate

ultimate is a game server with horizontally-scalable and high-available. It was powered by [go-micro](https://github.com/micro/go-micro) and running in docker container.

- using mysql as data persist.
- using [consul](https://www.consul.io) as server discovery and registry.
- using [grpc](https://grpc.io) as rpc call from each servers.
- using [nsq](https://nsq.io) as message transporter.
- using [loki/grafana](https://github.com/grafana/loki) as log storage and processing queries.

## Getting Started

* [Install](#install)
* [Run With Cmd](#run-with-cmd)
* [Run With Container](#run-with-container)
* [Monitor](#monitor)


## Install

clone this repo

	git clone https://github.com/hellodudu/Ultimate.git


## Run With Cmd

- nsq:

        nsqlookupd
        nsqd --lookupd-tcp-address=127.0.0.1:4160

- consul:

        consul agent -dev

- game-service:

        cd game-service
        go run main.go plugin.go --registry=consul --broker=nsq

- arena-service:

        cd arena-service
        go run main.go plugin.go --registry=consul --broker=nsq

## Run With Container

- plugin
    
  first please install docker log driver [plugin](https://github.com/grafana/loki/tree/master/cmd/docker-driver) for loki.

        docker plugin install  grafana/loki-docker-driver:latest --alias loki --grant-all-permissions

- build 

		make docker

- run

        docker-compose up


## Monitor

- **consul** can be watched by [https://localhost:8500](https://localhost:8500)
![screen_shot_consul](https://github.com/hellodudu/Ultimate/raw/master/docs/screen_shot_consul.png)

- **nsq** can be watched by [https://localhost:4171](https://localhost:4171)
![screen_shot_nsq](https://github.com/hellodudu/Ultimate/raw/master/docs/screen_shot_nsq.png)

- **grafana** can be watched by [https://localhost:3000](https://localhost:3000)
![screen_shot_grafana](https://github.com/hellodudu/Ultimate/raw/master/docs/screen_shot_grafana.png)

- **loki** can be watched after [adding data source](https://grafana.com/docs/features/datasources/loki/)
![screen_shot_grafana_add_loki_data_source](https://github.com/hellodudu/Ultimate/raw/master/docs/screen_shot_grafana_add_loki_data_source.png)
![screen_shot_loki_log](https://github.com/hellodudu/Ultimate/raw/master/docs/screen_shot_loki_log.png)
