#!/bin/zsh

autoload zargs
setopt extendedglob
cd ../

zargs -l -I{} -- *~util -- ln -s `pwd`/{} $HOME/.{}
