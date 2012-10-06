#!/bin/sh

g++ -c mainlib.cpp array.cpp filter.cpp draw.cpp -m32 `pkg-config --cflags opencv`
ghc --make $1 -O -prof -auto-all -lstdc++ mainlib.o array.o filter.o draw.o `pkg-config --libs opencv`
