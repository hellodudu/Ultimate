package utils

import (
	"github.com/go-ini/ini"
)

type IniMgr struct {
	mapIniFile map[string]*ini.File
}

func NewIniMgr() *IniMgr {
	return &IniMgr{mapIniFile: make(map[string]*ini.File, 256)}
}

// get ini file by path
func (im *IniMgr) getIniFile(name string) (*ini.File, error) {
	if f, ok := im.mapIniFile[name]; ok {
		return f, nil
	}

	f, err := ini.Load(name)
	if err == nil {
		im.mapIniFile[name] = f
		return f, nil
	}

	return nil, err
}

// get ini value by section and key
func (im *IniMgr) GetIniValue(name string, section string, key string) (string, error) {
	f, err := im.getIniFile(name)
	if err != nil {
		return "", err
	}

	s, err := f.GetSection(section)
	if err != nil {
		return "", err
	}

	k, err := s.GetKey(key)
	if err != nil {
		return "", err
	}

	return k.Value(), nil
}
