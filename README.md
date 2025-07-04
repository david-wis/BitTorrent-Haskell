Inspired by 
["Build Your Own BitTorrent" Challenge](https://app.codecrafters.io/courses/bittorrent/overview).

# Run with stack
```
stack run <file> <output_dir> <threads_per_peer> [<max_peers>]
```
Where:
- file: path to the .torrent file
- output_dir: path to the directory where the .part files and the final file will be downloaded
- thread_per_peer: max amount of threads used for each peer that answers the handshake correctly
- max_peers (optional): limits the peer list to a specific length

# Example
```sh
stack run file.torrent tests 100
```


# Run with Docker
Warning: 5gb image 
```sh
docker build -t hs-bittorrent .
```

```sh
docker run --rm -v "$(pwd)/app:/bittorrent/app" -v "$(pwd):/bittorrent/tests" hs-bittorrent <file> <output_dir> <threads_per_peer> [<max_peers>]
```

To watch the parts creation
```
watch 'ls <selected output directory> | wc'
```


## Example
```sh
docker run --rm -v "$(pwd)/app:/bittorrent/app" -v "$(pwd):/bittorrent/tests" hs-bittorrent /bittorrent/tests/file.torrent /bittorrent/tests 100
```