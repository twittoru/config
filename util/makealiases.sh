#!/bin/zsh

autoload zargs
cd ../

zargs -l -I{} -- *~util -- ln -s `pwd`/{} $HOME/.{}
