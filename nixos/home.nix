{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: let
  userNvim = lib.cleanSourceWith {
    src = ../neovim/.user.nvim;
    filter = path: _type: baseNameOf path != "lazy-lock.json";
  };
in {
  home.username = "wolfe";
  home.homeDirectory = "/home/wolfe";
  home.stateVersion = "25.11";

  # Have home manager manage itself
  programs.home-manager.enable = true;

  home.packages = with pkgs; [
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
    gnupg

    nixd
    alejandra
  ];

  programs.zsh = {
    enable = true;
    syntaxHighlighting.enable = true;
    initContent = builtins.readFile ../zsh/.zshrc;
  };

  home.file.bin.source = ../bin/bin;

  home.file.".user.nvim" = {
    source = userNvim;
    recursive = true;
  };
  home.file.".user.nvim/lazy-lock.json".source =
    config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/dotfiles/neovim/.user.nvim/lazy-lock.json";

  xdg.configFile."nvim".source = lib.cleanSource ../neovim/.nvim-shared;

  home.file.".tmux.conf".source = ../tmux/.tmux.conf;
  home.file.".gitconfig".source = ../git/.gitconfig;

  # SQLite shell history
  programs.atuin = {
    enable = true;
    enableZshIntegration = true;
    flags = ["--disable-up-arrow"];

    settings = {
      auto_sync = false;
      keymap_mode = "auto";
    };

    daemon.enable = true;
  };
}
