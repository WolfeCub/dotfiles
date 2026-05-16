{
  pkgs,
  inputs,
  lib,
  ...
}: {
  imports = [
    inputs.nixos-wsl.nixosModules.default
    ../shared/user.nix
  ];

  wsl.enable = true;
  wsl.defaultUser = "wolfe";
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";

  nix.settings.experimental-features = ["nix-command" "flakes"];

  time.timeZone = "America/Toronto";

  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = with pkgs; [
  ];

  system.stateVersion = "25.11";
}
