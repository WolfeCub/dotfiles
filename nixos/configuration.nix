{ lib, pkgs, config, ... }:

let
  unstableTarball = fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz;
in {

  imports = [
    <nixpkgs/nixos/modules/profiles/minimal.nix>
    ./wsl
  ];

  environment.noXlibs = false;

  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides = pkgs: {
      unstable = import unstableTarball {
        config = config.nixpkgs.config;
      };
    };
  };

  environment.systemPackages = with pkgs; [
    man-db
    manpages
    gnumake
    cmake
    gcc

    zsh
    git
    vim
    stow
    unstable.emacs
  ];

  fonts.fonts = with pkgs; [
    corefonts
    google-fonts
    fira-mono
  ];
}
