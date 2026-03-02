FROM haskell:9.6 AS builder

WORKDIR /opt/build

RUN cabal update

COPY cabal.project cabal.project.freeze ./
COPY packages packages

RUN cabal build all && cabal test all

RUN rm -rf packages cabal.project cabal.project.freeze

FROM builder
