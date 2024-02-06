#!/bin/bash

git clone https://github.com/Mulan-Szechuan-Sauce/nvim-config $HOME/.config/nvim

cd $HOME/dotfiles

git submodule init
git submodule update

ln -s $HOME/dotfiles/zsh/.zshrc $HOME
ln -s $HOME/dotfiles/zsh/.zsh $HOME

ln -s $HOME/dotfiles/neovim/.user.nvim $HOME

ln -s $HOME/dotfiles/tmux/.tmux.conf $HOME
