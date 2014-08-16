FROM alanz/haskell-ghc-7.8-64
RUN mkdir -p /srv
WORKDIR /srv
ENV PATH /.cabal/bin:/srv/.cabal-sandbox/bin:$PATH
ENV LANG C.UTF-8
RUN cabal update
RUN apt-get update
RUN apt-get install libpq-dev postgresql-client-9.3 -yy
RUN cabal install alex happy
ADD skilltree.cabal /srv/skilltree.cabal
ADD Makefile /srv/Makefile
RUN make init
RUN cabal install -j --only-dependencies
RUN cabal install -j  -fdevelopment
ADD . /srv
