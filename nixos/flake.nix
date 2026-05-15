{
  description = "NixOS Configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager.url = "github:nix-community/home-manager/release-25.11";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

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
  } @ inputs: {
    overlays = import ./overlays.nix {inherit inputs;};

    # NixOS configuration entrypoint
    # Available through 'nixos-rebuild --flake .#your-hostname'
    nixosConfigurations = {
      vital-nix-vm = nixpkgs.lib.nixosSystem {
        specialArgs = {inherit inputs;};
        modules = [
          {nixpkgs.overlays = builtins.attrValues self.overlays;}
          ./work/configuration.nix
        ];
      };
    };

    # Standalone home-manager configuration entrypoint
    # Available through 'home-manager --flake .#your-username@your-hostname'
    homeConfigurations = {
      "wolfe@vital-nix-vm" = home-manager.lib.homeManagerConfiguration {
        pkgs = import nixpkgs {
          system = "aarch64-linux";
          overlays = builtins.attrValues self.overlays;
        };
        extraSpecialArgs = {inherit inputs;};
        modules = [./home.nix];
      };
    };
  };
}
