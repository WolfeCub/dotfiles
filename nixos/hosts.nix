{
  inputs,
  lib,
  withSystem,
  dfRoot,
  ...
}: let
  inherit (inputs.self) nixosModules homeModules;

  # hostname -> system. Each host's modules are named after the hostname:
  # nixosModules.<host> and homeModules.<host>. pkgs (with overlays) comes
  # from the per-system pkgs defined in overlays.nix, via withSystem.
  hosts = {
    vital-nix-vm = "aarch64-linux";
    nixos = "x86_64-linux";
    darktower = "x86_64-linux";
  };

  mkNixos = name: system:
    withSystem system ({pkgs, ...}:
      inputs.nixpkgs.lib.nixosSystem {
        specialArgs = {inherit inputs;};

        modules = [
          {nixpkgs.pkgs = pkgs;}
          nixosModules.nh
          nixosModules.${name}
        ];
      });

  mkHome = name: system:
    withSystem system ({pkgs, ...}:
      inputs.home-manager.lib.homeManagerConfiguration {
        inherit pkgs;

        extraSpecialArgs = {inherit inputs dfRoot;};

        modules = [homeModules.${name}];
      });
in {
  flake.nixosConfigurations = lib.mapAttrs mkNixos hosts;

  flake.homeConfigurations =
    lib.mapAttrs'
    (name: system: lib.nameValuePair "wolfe@${name}" (mkHome name system))
    hosts;
}
