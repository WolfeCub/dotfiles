#!/bin/bash

REPOPATH="$(find ~ -name dotfiles -type d)"

if [ -z "$REPOPATH" ]; then
    echo "Could not find dotfiles repository"
    exit 1
else
    echo "Found dotfiles at: $REPOPATH"
fi

NVIMPATH="$HOME/.config/nvim"
if [ -d "$NVIMPATH" ]; then
    cd $NVIMPATH
    echo "Found nvim config at: $NVIMPATH. Updating..."
    git pull
else
    echo "Cloning nvim config to: $NVIMPATH"
    git clone https://github.com/Mulan-Szechuan-Sauce/nvim-config $NVIMPATH
fi

cd $REPOPATH

echo "Initializing and updating submodules"
git submodule init
git submodule update

echo "Linking zsh"
ln -f -s $REPOPATH/zsh/.zshrc $HOME
ln -f -s $REPOPATH/zsh/.zsh $HOME

echo "Linking user nvim"
ln -f -s $REPOPATH/neovim/.user.nvim $HOME

echo "Linking tmux"
ln -f -s $REPOPATH/tmux/.tmux.conf $HOME
