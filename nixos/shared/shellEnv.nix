{pkgs, dfRoot, ...}: {
  home.username = "wolfe";
  home.homeDirectory = "/home/wolfe";
  home.stateVersion = "25.11";

  # Have home manager manage itself
  programs.home-manager.enable = true;

  home.packages = with pkgs; [
    zsh
    tmux
    vim
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
    jq

    nixd
    alejandra
  ];

  programs.zsh = {
    enable = true;
    syntaxHighlighting.enable = true;
    initContent = builtins.readFile (dfRoot + /zsh/.zshrc);
  };

  home.file.bin.source = dfRoot + /bin/bin;

  home.file.".tmux.conf".source = (dfRoot + /tmux/.tmux.conf);
  home.file.".gitconfig".source = (dfRoot + /git/.gitconfig);

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
