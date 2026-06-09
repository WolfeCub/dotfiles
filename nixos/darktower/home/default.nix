{
  pkgs,
  inputs,
  ...
}: {
  imports = [
    inputs.nixcord.homeModules.nixcord

    ../../shared/shellEnv.nix
    ../../shared/neovim.nix
    ../../shared/rio.nix

    ./fonts.nix
    ./niri.nix
    ./noctalia.nix
  ];

  nixpkgs.config.allowUnfree = true;
  home.packages = with pkgs; [
    rio
    firefox-devedition
  ];

  programs.nixcord = {
    enable = true;
    discord.vencord.enable = true;
    discord.krisp.enable = true;
  };
}
