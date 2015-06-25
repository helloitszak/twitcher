package config

import (
	"encoding/json"
	"io/ioutil"
)

type Config struct {
	ClientId     string
	ClientSecret string
	Socket       string
}

func LoadKey(path string) (string, error) {
	b, err := ioutil.ReadFile(path)
	if err != nil {
		return "", err
	}

	return string(b), nil
}

func SaveKey(path string, key string) error {
	err := ioutil.WriteFile(path, []byte(key), 0700)
	return err
}

func LoadConfig(path string) (Config, error) {
	config := &Config{}
	b, err := ioutil.ReadFile(path)
	if err != nil {
		return *config, err
	}

	err2 := json.Unmarshal(b, config)
	if err2 != nil {
		return *config, err2
	}

	return *config, nil
}
