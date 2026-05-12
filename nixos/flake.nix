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

  outputs =
    {
      self,
      nixpkgs,
      home-manager,
      ...
    }@inputs:
    {
      overlays = import ./overlays.nix { inherit inputs; };

      # NixOS configuration entrypoint
      # Available through 'nixos-rebuild --flake .#your-hostname'
      nixosConfigurations = {
        vital-nix-vm = nixpkgs.lib.nixosSystem {
          modules = [
            { nixpkgs.overlays = builtins.attrValues self.overlays; }
            ./work/configuration.nix
          ];
        };
      };

      # Standalone home-manager configuration entrypoint
      # Available through 'home-manager --flake .#your-username@your-hostname'
      # homeConfigurations = {
      #   # FIXME replace with your username@hostname
      #   "your-username@your-hostname" = home-manager.lib.homeManagerConfiguration {
      #     # Home-manager requires 'pkgs' instance
      #     pkgs = nixpkgs.legacyPackages.x86_64-linux; # FIXME replace x86_64-linux with your architecture
      #     extraSpecialArgs = {inherit inputs;};
      #     # > Our main home-manager configuration file <
      #     modules = [./home-manager/home.nix];
      #   };
      # };
    };
}
