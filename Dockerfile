FROM haskell:9.4.7
RUN ln -s $(which ghc) /usr/bin/ghc-9.4.7
WORKDIR /usr/aoc-2023

COPY aoc2023.cabal ./
COPY cabal.project ./
RUN cabal update
RUN cabal build --only-dependencies

COPY . .
RUN cabal build

ENTRYPOINT [ "./day" ]
