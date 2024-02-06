#!/bin/bash

REPOPATH="$(find ~ -name dotfiles -type d)"
echo "Found dotfiles at: $REPOPATH"

if [ -z "$REPOPATH" ]; then
    echo "Could not find dotfiles repository"
    exit 1
fi

git clone https://github.com/Mulan-Szechuan-Sauce/nvim-config $HOME/.config/nvim

cd $HOME/dotfiles

git submodule init
git submodule update

ln -s $HOME/dotfiles/zsh/.zshrc $HOME
ln -s $HOME/dotfiles/zsh/.zsh $HOME

ln -s $HOME/dotfiles/neovim/.user.nvim $HOME

ln -s $HOME/dotfiles/tmux/.tmux.conf $HOME
