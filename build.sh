#!/bin/sh

g++ -c mainlib.cpp array.cpp filter.cpp -m32 `pkg-config --cflags opencv`
ghc --make $1 -O -prof -auto-all -lstdc++ mainlib.o array.o filter.o  `pkg-config --libs opencv`
