{inputs, ...}: {
  flake.nixosModules.nixos = {
    pkgs,
    lib,
    ...
  }: {
    imports = [
      inputs.nixos-wsl.nixosModules.default
      inputs.self.nixosModules.user
    ];

    wsl.enable = true;
    wsl.defaultUser = "wolfe";
    wsl.wslConf.interop.appendWindowsPath = false;

    nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";

    nix.settings.experimental-features = ["nix-command" "flakes"];

    time.timeZone = "America/Toronto";

    nixpkgs.config.allowUnfree = true;
    environment.systemPackages = with pkgs; [
    ];

    programs.nix-ld.enable = true;

    system.stateVersion = "25.11";
  };
}
