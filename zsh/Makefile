#!/usr/bin/make 
__ALL_SRC__ := $(wildcard *.zsh) .zshenv .zshrc
TARGET:=$(__ALL_SRC__:%=%.zwc)

all: $(TARGET)

%.zwc: %
    zsh -fc "zcompile $*"

clean:
    -@rm -f $(TARGET)
