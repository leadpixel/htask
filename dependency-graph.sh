#!/usr/bin/env sh

find packages/ -name '*.hs' | grep -v test | xargs graphmod -q | xdot /dev/stdin
