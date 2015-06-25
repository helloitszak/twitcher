package twitch

import (
	"encoding/json"
	"fmt"
	"github.com/braintree/manners"
	"io/ioutil"
	"net/http"
	"net/url"
)

type TokenResponse struct {
	AccessToken string   `json:"access_token"`
	Scope       []string `json:"scope"`
}

type FollowedStreamsResponse struct {
	Streams []Stream `json:"streams"`
}

type Stream struct {
	Game        string  `json:"game"`
	Viewers     int     `json:"viewers"`
	AverageFps  float32 `json:"average_fps"`
	VideoHeight int     `json:"video_height"`
	Channel     Channel `json:"channel"`
}

type Channel struct {
	Status      string `json:"status"`
	DisplayName string `json:"display_name"`
	Url         string `json:"url"`
}

func GetFollowedStreams(token string) (FollowedStreamsResponse, error) {
	var fr FollowedStreamsResponse
	client := &http.Client{}

	req, err := http.NewRequest("GET", "https://api.twitch.tv/kraken/streams/followed?limit=100", nil)
	if err != nil {
		return fr, err
	}

	req.Header.Set("Accept", "application/vnd.twitchtv.v3+json")
	req.Header.Set("Authorization", "OAuth "+token)

	res, err := client.Do(req)
	if err != nil {
		return fr, err
	}

	body, err := ioutil.ReadAll(res.Body)
	if err != nil {
		return fr, err
	}

	err2 := json.Unmarshal(body, &fr)
	if err2 != nil {
		return fr, err2
	}

	return fr, nil
}

func GetUserToken(client_id string, client_secret string, socket string) (string, error) {
	mux := http.NewServeMux()

	result := make(chan string)
	error := make(chan error)

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
			error <- err
		}

		contents, err := ioutil.ReadAll(resp.Body)
		if err != nil {
			error <- err
		}

		var tokenResp TokenResponse
		err2 := json.Unmarshal(contents, &tokenResp)

		if err2 != nil {
			error <- err2
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

	select {
	case r := <-result:
		return r, nil
	case e := <-error:
		return "", e
	}
}
