{
  description = "NixOS Configuration";

  inputs = {
    self.submodules = true;

    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager.url = "github:nix-community/home-manager/release-25.11";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    nixos-wsl = {
      url = "github:nix-community/NixOS-WSL/main";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    lspmux = {
      url = "git+https://codeberg.org/p2502/lspmux/";
      flake = false;
    };
  };

  outputs = {
    self,
    nixpkgs,
    home-manager,
    nixos-wsl,
    ...
  } @ inputs: let
    hosts = {
      vital-nix-vm = {
        system = "aarch64-linux";
        nixosModule = ./work/configuration.nix;
      };

      nixos = {
        system = "x86_64-linux";
        nixosModule = ./wsl/configuration.nix;
      };
    };

    mkHost = name: cfg:
      nixpkgs.lib.nixosSystem {
        specialArgs = {inherit inputs;};

        modules = [
          {nixpkgs.overlays = builtins.attrValues self.overlays;}
          ./shared/packages.nix
          ./shared/nh.nix
          cfg.nixosModule
        ];
      };
    mkHome = name: cfg:
      home-manager.lib.homeManagerConfiguration {
        pkgs = import nixpkgs {
          system = cfg.system;
          overlays = builtins.attrValues self.overlays;
        };

        extraSpecialArgs = {
          inherit inputs;
        };

        modules = [
          ./home.nix
        ];
      };
  in {
    overlays = import ./overlays.nix {inherit inputs;};

    nixosConfigurations = builtins.mapAttrs mkHost hosts;
    homeConfigurations =
      nixpkgs.lib.mapAttrs' (host: cfg: {
        name = "wolfe@${host}";
        value = mkHome host cfg;
      })
      hosts;
  };
}
