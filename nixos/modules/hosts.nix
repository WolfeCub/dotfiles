{
  inputs,
  lib,
  ...
}: let
  inherit (inputs.self) nixosModules homeModules;

  overlays = builtins.attrValues (import ../overlays.nix {inherit inputs;});

  # hostname -> system. Each host's modules are named after the hostname:
  # nixosModules.<host> and homeModules.<host>.
  hosts = {
    vital-nix-vm = "aarch64-linux";
    nixos = "x86_64-linux";
    darktower = "x86_64-linux";
  };

  mkNixos = name: _system:
    inputs.nixpkgs.lib.nixosSystem {
      specialArgs = {inherit inputs;};

      modules = [
        {nixpkgs.overlays = overlays;}
        nixosModules.nh
        nixosModules.${name}
      ];
    };

  mkHome = name: system:
    inputs.home-manager.lib.homeManagerConfiguration {
      pkgs = import inputs.nixpkgs {inherit system overlays;};

      extraSpecialArgs = {
        inherit inputs;
        dfRoot = ../..;
      };

      modules = [homeModules.${name}];
    };
in {
  flake.nixosConfigurations = lib.mapAttrs mkNixos hosts;

  flake.homeConfigurations =
    lib.mapAttrs'
    (name: system: lib.nameValuePair "wolfe@${name}" (mkHome name system))
    hosts;
}
