{
  description = "NixOS Configuration";

  inputs = {
    self.submodules = true;

    nixpkgs.url = "github:nixos/nixpkgs/nixos-26.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager.url = "github:nix-community/home-manager/release-26.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    nix-wrapper-modules.url = "github:BirdeeHub/nix-wrapper-modules";
    nix-wrapper-modules.inputs.nixpkgs.follows = "nixpkgs";

    niri = {
      url = "github:sodiboo/niri-flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    noctalia = {
      url = "github:noctalia-dev/noctalia";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixos-wsl = {
      url = "github:nix-community/NixOS-WSL/main";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixcord = {
      url = "github:FlameFlag/nixcord";
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
    ...
  } @ inputs: let
    hosts = {
      vital-nix-vm = {
        system = "aarch64-linux";
        path = ./work;
      };

      nixos = {
        system = "x86_64-linux";
        path = ./wsl;
      };

      darktower = {
        system = "x86_64-linux";
        path = ./darktower;
      };
    };

    # Prefer a directory (`base`) over a sibling `.nix` file.
    pickPath = base:
      if builtins.pathExists base
      then base
      else base + ".nix";

    mkHost = name: cfg:
      nixpkgs.lib.nixosSystem {
        specialArgs = {inherit inputs;};

        modules = [
          {nixpkgs.overlays = builtins.attrValues self.overlays;}
          ./shared/nh.nix
          (cfg.path + "/configuration")
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
          dfRoot = ../.;
        };

        modules = [
          (pickPath (cfg.path + "/home"))
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
