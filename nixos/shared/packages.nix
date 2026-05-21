{pkgs, ...}: {
  environment.systemPackages = with pkgs; [
    zsh
    tmux
    vim
    unstable.neovim
    gitFull
    gcc
    ripgrep
    fd
    fzf
    btop

    nixd
    alejandra
  ];
}
