#!/bin/bash

REPOPATH="$(find ~ -name dotfiles -type d)"
echo "Found dotfiles at: $REPOPATH"

if [ -z "$REPOPATH" ]; then
    echo "Could not find dotfiles repository"
    exit 1
fi

NVIMPATH="$HOME/.config/nvim"
if [ -d "$NVIMPATH" ]; then
    cd $NVIMPATH
    git pull
else
    git clone https://github.com/Mulan-Szechuan-Sauce/nvim-config $NVIMPATH
fi

cd $REPOPATH

git submodule init
git submodule update

ln -s $REPOPATH/zsh/.zshrc $HOME
ln -s $REPOPATH/zsh/.zsh $HOME

ln -s $REPOPATH/neovim/.user.nvim $HOME

ln -s $REPOPATH/tmux/.tmux.conf $HOME
