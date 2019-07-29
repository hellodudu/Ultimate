package router

import (
	"github.com/google/uuid"
	"github.com/micro/go-micro/network/router/table"
	"github.com/micro/go-micro/registry"
)

var (
	// DefaultAddress is default router address
	DefaultAddress = ":9093"
	// DefaultNetwork is default micro network
	DefaultNetwork = "go.micro"
)

// Options are router options
type Options struct {
	// Id is router id
	Id string
	// Address is router address
	Address string
	// Gateway is micro network gateway
	Gateway string
	// Network is micro network
	Network string
	// Registry is the local registry
	Registry registry.Registry
	// Table is routing table
	Table table.Table
}

// Id sets Router Id
func Id(id string) Option {
	return func(o *Options) {
		o.Id = id
	}
}

// Address sets router service address
func Address(a string) Option {
	return func(o *Options) {
		o.Address = a
	}
}

// Gateway sets network gateway
func Gateway(g string) Option {
	return func(o *Options) {
		o.Gateway = g
	}
}

// Network sets router network
func Network(n string) Option {
	return func(o *Options) {
		o.Network = n
	}
}

// Table sets the routing table
func Table(t table.Table) Option {
	return func(o *Options) {
		o.Table = t
	}
}

// Registry sets the local registry
func Registry(r registry.Registry) Option {
	return func(o *Options) {
		o.Registry = r
	}
}

// DefaultOptions returns router default options
func DefaultOptions() Options {
	return Options{
		Id:       uuid.New().String(),
		Address:  DefaultAddress,
		Network:  DefaultNetwork,
		Registry: registry.DefaultRegistry,
		Table:    table.NewTable(),
	}
}
