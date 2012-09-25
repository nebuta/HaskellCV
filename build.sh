#!/bin/sh

g++ -c mainlib.cpp array.cpp -m32 `pkg-config --cflags opencv`
ghc --make $1 -lstdc++ mainlib.o array.o `pkg-config --libs opencv`
