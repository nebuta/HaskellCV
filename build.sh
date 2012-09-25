#!/bin/sh

g++ -c mainlib.cpp array.cpp -m32 `pkg-config --cflags opencv`
ghc --make main.hs -lstdc++ mainlib.o array.o `pkg-config --libs opencv`
