# Twitcher

Check in on your twitch follows and jump directly to their stream without
leaving the comfort of your terminal.

## Usage
Once installed this has to be configured with a secret key and id and socket
from Twitch's API. I do not distribute any key with the program, so you're on
your own for this.

Just place a file in `~/.config/twitcher/config.json` (on mac/linux) with the
following in it:

```json
{
"ClientId": "",
"ClientSecret": "",
"Socket": ""
}
```

I promise I'll make this experience better some day.

## License
See LICENSE file for details
