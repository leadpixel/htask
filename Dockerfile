FROM haskell:8.6.3

RUN stack setup 8.6.3 && stack update --resolver 13.10

COPY stack.yaml .
COPY packages packages
COPY modules modules

RUN stack build && stack test && stack clean
