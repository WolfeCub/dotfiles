{pkgs, ...}: {
  environment.systemPackages = with pkgs; [
    zsh
    tmux
    vim
    unstable.neovim
    gitFull
    unstable.delta
    unstable.gh
    gcc
    ripgrep
    fd
    fzf
    direnv
    btop

    nixd
    alejandra

    kubectl
    kubectx
  ];
}
