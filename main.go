package main

import (
	"encoding/json"
	"fmt"
	"github.com/braintree/manners"
	"github.com/ubercow/twitcher/util"
	"io/ioutil"
	"log"
	"net/http"
	"net/url"
	"os"
	"path/filepath"
	"runtime"
	"strconv"
)

type twitchTokenResponse struct {
	AccessToken string   `json:"access_token"`
	Scope       []string `json:"scope"`
}

type twitchFollowedStreamsResponse struct {
	Streams []twitchStream `json:"streams"`
}

type twitchStream struct {
	Game        string        `json:"game"`
	Viewers     int           `json:"viewers"`
	AverageFps  float32       `json:"average_fps"`
	VideoHeight int           `json:"video_height"`
	Channel     twitchChannel `json:"channel"`
}

type twitchChannel struct {
	DisplayName string `json:"display_name"`
	Url         string `json:"url"`
}

type settings struct {
	ClientId     string
	ClientSecret string
	ServerSocket string
	ClientToken  string
}

func getUserToken(client_id string, client_secret string, socket string) string {
	mux := http.NewServeMux()

	result := make(chan string)

	redirect_uri := "http://" + socket + "/oauth"

	mux.HandleFunc("/oauth", func(w http.ResponseWriter, r *http.Request) {
		code := r.URL.Query().Get("code")
		if code == "" {
			w.WriteHeader(400)
			fmt.Fprintln(w, "You must specify code in querystring. Probably Oauth error...")
		}

		v := url.Values{}
		v.Set("client_id", client_id)
		v.Set("client_secret", client_secret)
		v.Set("grant_type", "authorization_code")
		v.Set("redirect_uri", redirect_uri)
		v.Set("code", code)

		resp, err := http.PostForm("https://api.twitch.tv/kraken/oauth2/token", v)
		if err != nil {
			log.Fatal("Getting token: ", err)
		}

		contents, err := ioutil.ReadAll(resp.Body)
		if err != nil {
			log.Fatal("Reading body: ", err)
		}

		var tokenResp twitchTokenResponse
		err2 := json.Unmarshal(contents, &tokenResp)

		if err2 != nil {
			log.Fatal("Decoding JSON: ", err)
		}
		w.Write([]byte("we got the token, close this now"))

		result <- tokenResp.AccessToken
	})

	mux.HandleFunc("/authorize", func(w http.ResponseWriter, r *http.Request) {

		v := url.Values{}
		v.Set("client_id", client_id)
		v.Set("response_type", "code")
		v.Set("redirect_uri", redirect_uri)
		v.Set("scope", "user_read")

		url := "https://api.twitch.tv/kraken/oauth2/authorize?" + v.Encode()
		http.Redirect(w, r, url, 302)
	})

	s := manners.NewWithServer(&http.Server{
		Addr:    socket,
		Handler: mux,
	})
	defer s.Close()

	go func() {
		fmt.Println("Visit http://" + socket + "/authorize to get a key")
		s.ListenAndServe()
	}()

	return <-result
}

type config struct {
	ClientId     string
	ClientSecret string
	Socket       string
}

func configDir() string {
	if runtime.GOOS == "windows" {
		return filepath.Join(os.Getenv("APPDATA"), "twitcher")
	}
	if xdg := os.Getenv("XDG_CONFIG_HOME"); xdg != "" {
		return filepath.Join(xdg, "twitcher")
	}

	return filepath.Join(os.Getenv("HOME"), ".config", "twitcher")
}

func loadConfig(path string) (config, error) {
	config := &config{}
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

func loadKey(path string) (string, error) {
	b, err := ioutil.ReadFile(path)
	if err != nil {
		return "", err
	}

	return string(b), nil
}

func saveKey(path string, key string) error {
	err := ioutil.WriteFile(path, []byte(key), 0700)
	return err
}

func main() {
	forceAuthenticate := false

	keyPath := filepath.Join(configDir(), "key")
	key, err := loadKey(keyPath)
	if os.IsNotExist(err) || forceAuthenticate {
		config, err := loadConfig(filepath.Join(configDir(), "config.json"))
		if err != nil {
			log.Fatal("Loading config:", err)
		}

		key = getUserToken(config.ClientId, config.ClientSecret, config.Socket)
		err2 := saveKey(keyPath, key)
		if err2 != nil {
			log.Println("error saving key", err2)
		}
	} else if err != nil {
		log.Fatal("loading key: ", err)
	}
	client := &http.Client{}
	req, _ := http.NewRequest("GET", "https://api.twitch.tv/kraken/streams/followed?limit=100", nil)
	req.Header.Set("Accept", "application/vnd.twitchtv.v3+json")
	req.Header.Set("Authorization", "OAuth "+key)
	res, _ := client.Do(req)
	body, _ := ioutil.ReadAll(res.Body)

	var fr twitchFollowedStreamsResponse
	json.Unmarshal(body, &fr)

	thing := make([][]string, len(fr.Streams))

	pads := []util.PadMode{
		util.LeftPad,
		util.LeftPad,
		util.RightPad,
		util.RightPad,
		util.RightPad,
		util.LeftPad,
		util.RightPad,
		util.RightPad}
	for i, s := range fr.Streams {
		thing[i] = []string{
			strconv.Itoa(i+1) + ".",
			s.Channel.DisplayName,
			"-",
			s.Game,
			"-",
			strconv.Itoa(s.Viewers),
			"viewers -",
			strconv.Itoa(s.VideoHeight) + "p"}
	}

	ss := util.FormatStreams(thing, pads)

	for _, s := range ss {
		fmt.Println(s)
	}
	num := util.Pick(len(fr.Streams))

	util.StartProgram("livestreamer", fr.Streams[num-1].Channel.Url)
}
