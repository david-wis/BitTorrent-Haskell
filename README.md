[![progress-banner](https://backend.codecrafters.io/progress/bittorrent/fe5b9f7c-8dc1-4086-8b8d-e3f83f4285f4)](https://app.codecrafters.io/users/codecrafters-bot?r=2qF)

Inspired by 
["Build Your Own BitTorrent" Challenge](https://app.codecrafters.io/courses/bittorrent/overview).

BitTorrent client that's capable of parsing a
.torrent file and downloading a file from a peer. Along the way, we’ll learn
about how torrent files are structured, HTTP trackers, BitTorrent’s Peer
Protocol, pipelining and more.


# Run with Docker
```sh
docker build -t hs-bittorrent .
```

```sh
docker run --rm -v "$(pwd)/app:/bittorrent/app" hs-bittorrent decode 6:lambda
```

```sh
docker run --rm -v "$(pwd)/app:/bittorrent/app" -v "$(pwd):/bittorrent/tests" hs-bittorrent info /bittorrent/tests/sample.torrent
```