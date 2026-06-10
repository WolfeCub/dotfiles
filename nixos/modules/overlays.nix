{inputs, ...}: let
  overlays = [
    # Custom packages from this repo, exposed as pkgs.customPkgs.*
    (final: _prev: {
      customPkgs = {
        lspmux = final.callPackage (inputs.lspmux + "/package.nix") {};
      };
    })

    # nixpkgs-unstable, accessible as pkgs.unstable
    (final: _prev: {
      unstable = import inputs.nixpkgs-unstable {
        inherit (final.stdenv.hostPlatform) system;
        config.allowUnfree = true;
      };
    })
  ];
in {
  # The single place overlays + config are applied. Every consumer draws from
  # this per-system pkgs: perSystem packages directly, and the nixos/home
  # configs via `withSystem` in hosts.nix.
  perSystem = {system, ...}: {
    _module.args.pkgs = import inputs.nixpkgs {
      inherit system overlays;
      config.allowUnfree = true;
    };
  };
}
