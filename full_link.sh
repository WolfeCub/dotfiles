#!/bin/sh

mkdir -p ~/.config
mkdir -p ~/.config

ln -sd `pwd`/bspwm ~/.config
ln -sd `pwd`/sxhkd ~/.config
ln -s `pwd`/.Xresources ~

ln -sd `pwd`/.vim ~
ln -s `pwd`/.vimrc ~
ln -sd `pwd`/.emacs.d ~
ln -s `pwd`/.tmux.conf ~
ln -sd `pwd`/zsh ~
ln -s `pwd`/zshrc ~
ln -s `pwd`/Xmodmap ~
