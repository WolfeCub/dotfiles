{
  pkgs,
  inputs,
  ...
}: {
  imports = [
    inputs.niri.homeModules.config
    inputs.noctalia.homeModules.default

    ../../shared/shellEnv.nix
    ../../shared/neovim.nix
    ../../shared/rio.nix

    ./fonts.nix
    ./desktop.nix
  ];

  nixpkgs.config.allowUnfree = true;
  home.packages = with pkgs; [
    rio
    firefox-devedition
    discord
  ];
}
