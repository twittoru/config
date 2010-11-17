#!/bin/sh

for filename in emacs stumpwmrc vimperatorrc
do
    ln -s `pwd`/dot.$filename $HOME/.$filename
done
