# Arena-Service Service

This is the Arena-Service service

Generated with

```
micro new github.com/hellodudu/Ultimate/arena-service --namespace=ultimate.service --type=arena
```

## Getting Started

- [Configuration](#configuration)
- [Dependencies](#dependencies)
- [Usage](#usage)

## Configuration

- FQDN: ultimate.service.arena
- Type: srv
- Alias: arena-service

## Dependencies

Micro services depend on service discovery. The default is multicast DNS, a zeroconf system.

In the event you need a resilient multi-host setup we recommend consul.

```
# install consul
brew install consul

# run consul
consul agent -dev
```

## Usage

A Makefile is included for convenience

Build the binary

```
make build
```

Run the service
```
./ultimate-service-arena
```

Build a docker image
```
make docker
```
