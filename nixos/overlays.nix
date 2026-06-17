{inputs, ...}: {
  # The single place overlays + config are applied. Every consumer draws from
  # this per-system pkgs: perSystem packages directly, and the nixos/home
  # configs via `withSystem` in hosts.nix.
  perSystem = {system, ...}: {
    _module.args.pkgs = import inputs.nixpkgs {
      inherit system;
      config.allowUnfree = true;

      # nixpkgs-unstable, accessible as pkgs.unstable
      overlays = [
        (final: _prev: {
          unstable = import inputs.nixpkgs-unstable {
            inherit (final.stdenv.hostPlatform) system;
            config.allowUnfree = true;
          };
        })

        inputs.nix-cachyos-kernel.overlays.pinned
      ];
    };
  };
}
