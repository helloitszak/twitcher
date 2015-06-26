package main

import (
	"fmt"
	"github.com/ubercow/twitcher/config"
	"github.com/ubercow/twitcher/twitch"
	"github.com/ubercow/twitcher/util"
	"gopkg.in/alecthomas/kingpin.v2"
	"log"
	"os"
	"path/filepath"
	"runtime"
	"strconv"
)

func configDir() string {
	if runtime.GOOS == "windows" {
		return filepath.Join(os.Getenv("APPDATA"), "twitcher")
	}
	if xdg := os.Getenv("XDG_CONFIG_HOME"); xdg != "" {
		return filepath.Join(xdg, "twitcher")
	}

	return filepath.Join(os.Getenv("HOME"), ".config", "twitcher")
}

func getKey(cf string) (string, error) {
	config, err := config.LoadConfig(cf)
	if err != nil {
		return "", err
	}

	key, err := twitch.GetUserToken(config.ClientId, config.ClientSecret, config.Socket)
	if err != nil {
		return "", err
	}

	return key, nil
}

func consolePicker(streams []twitch.Stream) twitch.Stream {
	sets := make([][]util.PadSet, len(streams))
	for i, s := range streams {
		sets[i] = []util.PadSet{
			*util.P(strconv.Itoa(i+1)+".", util.LeftPad),
			*util.P(s.Channel.DisplayName, util.LeftPad),
			*util.P("-", util.RightPad),
			*util.P(util.Truncate(s.Channel.Status, 50), util.RightPad),
			*util.P("-", util.RightPad),
			*util.P(s.Game, util.RightPad),
			*util.P("-", util.RightPad),
			*util.P(strconv.Itoa(s.Viewers), util.LeftPad),
			*util.P("viewers -", util.RightPad),
			*util.P(strconv.Itoa(s.VideoHeight)+"p", util.RightPad)}

	}

	ss := util.PadFormat(sets)

	for _, s := range ss {
		fmt.Println(s)
	}
	num := util.Pick(len(streams))
	return streams[num-1]
}

func main() {
	forceAuthenticate := kingpin.Flag("auth", "Force Authenticate.").Default("false").Short('a').Bool()
	program := kingpin.Flag("program", "Launch this instead of web browser").Default("").Short('p').String()

	kingpin.Parse()

	configPath := filepath.Join(configDir(), "config.json")
	keyPath := filepath.Join(configDir(), "key")

	key, err := config.LoadKey(keyPath)
	if os.IsNotExist(err) || *forceAuthenticate {
		key, err = getKey(configPath)
		if err != nil {
			log.Fatal("getting key:", err)
		}

		err2 := config.SaveKey(keyPath, key)
		if err2 != nil {
			log.Println("error saving key", err2)
		}
	} else if err != nil {
		log.Fatal("loading key:", err)
	}

	if key == "" {
		log.Fatal("we could not get or load a key and one is required")
	}

	fr, err := twitch.GetFollowedStreams(key)
	if err != nil {
		log.Fatal("error getting twitch streams", err)
	}

	stream := consolePicker(fr.Streams)

	if *program != "" {
		fmt.Println("Starting", *program)
		util.StartProgram(*program, stream.Channel.Url)
	} else if tp := os.Getenv("TWITCHER_PROGRAM"); tp != "" {
		util.StartProgram(tp, stream.Channel.Url)
	} else {
		p, err := util.OpenCommand(stream.Channel.Url)
		if err != nil {
			log.Fatal(err)
		}

		p.Start()
	}
}
