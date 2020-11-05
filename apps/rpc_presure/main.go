package main

import (
	"flag"
	"fmt"
	"log"
	"os"
	"path/filepath"
	"sync"
	"syscall"

	"github.com/BurntSushi/toml"
	"github.com/hellodudu/Ultimate/internal/rpc_presure"
	"github.com/hellodudu/Ultimate/utils"
	"github.com/judwhite/go-svc/svc"
	"github.com/mreiferson/go-options"
)

type program struct {
	once sync.Once
	r    *rpc_presure.RPCPresure
}

func rpcPresureFlagSet(opts *rpc_presure.Options) *flag.FlagSet {
	flagSet := flag.NewFlagSet("rpc_presure", flag.ExitOnError)

	flagSet.Int("times", 1000, "how many rpc calls during 1 seconds")

	return flagSet
}

func main() {
	prg := &program{}
	if err := svc.Run(prg, syscall.SIGHUP, syscall.SIGQUIT, syscall.SIGINT, syscall.SIGTERM); err != nil {
		log.Fatal("%s", err)
	}
}

func (p *program) Init(env svc.Environment) error {
	if env.IsWindowsService() {
		dir := filepath.Dir(os.Args[0])
		return os.Chdir(dir)
	}
	return nil
}

func (p *program) Start() error {
	opts := rpc_presure.NewOptions()

	flagSet := rpcPresureFlagSet(opts)
	flagSet.Parse(os.Args[1:])

	var cfg map[string]interface{}
	configFlag := flagSet.Lookup("config")
	if configFlag != nil {
		configFile := configFlag.Value.String()
		if configFile != "" {
			_, err := toml.DecodeFile(configFile, &cfg)
			if err != nil {
				fmt.Errorf("failed to load config file %s - %s", configFile, err)
			}
		}
	}

	options.Resolve(opts, flagSet, cfg)
	r, err := rpc_presure.New(opts)
	if err != nil {
		fmt.Errorf("failed to instantiate rpc_presure", err)
	}
	p.r = r

	go func() {
		defer utils.CaptureException()
		err := p.r.Main()
		if err != nil {
			p.Stop()
			os.Exit(1)
		}
	}()

	return nil
}

func (p *program) Stop() error {
	p.once.Do(func() {
		p.r.Exit()
	})
	return nil
}
