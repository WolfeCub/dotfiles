{ config, pkgs, ... }:
{
  home.username = "wolfe";
  home.homeDirectory = "/home/wolfe";
  home.stateVersion = "25.11";

  # Have home manager manage itself
  programs.home-manager.enable = true;

  home.file.".user.nvim".source = ../neovim/.user.nvim;
}
