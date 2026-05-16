{
  config,
  pkgs,
  ...
}: {
  home.username = "wolfe";
  home.homeDirectory = "/home/wolfe";
  home.stateVersion = "25.11";

  # Have home manager manage itself
  programs.home-manager.enable = true;

  programs.zsh = {
    enable = true;
    syntaxHighlighting.enable = true;
    initContent = builtins.readFile ../zsh/.zshrc;
  };

  home.file.".user.nvim".source = ../neovim/.user.nvim;
  home.file.".tmux.conf".source = ../tmux/.tmux.conf;
  home.file.".gitconfig".source = ../git/.gitconfig;
}
