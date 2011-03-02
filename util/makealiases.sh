#!/bin/zsh

for filename in *(.)
do
    ln -s `pwd`/$filename $HOME/.$filename
done
