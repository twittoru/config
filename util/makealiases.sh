#!/bin/zsh

autoload zargs
setopt extendedglob
d ../

zargs -l -I{} -- *~util -- ln -s `pwd`/{} $HOME/.{}
