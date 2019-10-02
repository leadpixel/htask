FROM haskell:8.6.3

RUN stack setup 8.6.3
RUN stack update --resolver 13.10

COPY stack.yaml .
COPY packages packages
COPY modules modules
RUN stack build
RUN stack clean
