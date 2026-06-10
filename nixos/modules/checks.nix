{
  lib,
  config,
  ...
}: {
  flake.checks = lib.mkMerge (
    (lib.mapAttrsToList (name: cfg: {
        ${cfg.config.nixpkgs.hostPlatform.system}."nixos-${name}" =
          cfg.config.system.build.toplevel;
      })
      config.flake.nixosConfigurations)
    ++ (lib.mapAttrsToList (name: cfg: {
        ${cfg.pkgs.stdenv.hostPlatform.system}."home-${name}" =
          cfg.activationPackage;
      })
      config.flake.homeConfigurations)
  );
}
