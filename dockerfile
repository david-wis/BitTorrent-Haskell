FROM haskell:8.10

WORKDIR /bittorrent

COPY stack.yaml /bittorrent/stack.yaml
COPY package.yaml /bittorrent/package.yaml
COPY app /bittorrent/app

RUN stack install --resolver lts-21.11 --install-ghc

ENTRYPOINT ["stack", "run"]
CMD []