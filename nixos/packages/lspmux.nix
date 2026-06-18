{inputs, ...}: {
  # Buildable / runnable as `.#lspmux` (e.g. nix run github:WolfeCub/dotfiles#lspmux).
  perSystem = {pkgs, ...}: {
    packages.lspmux = pkgs.callPackage (inputs.lspmux + "/package.nix") {};
  };

  flake.nixosModules.lspmux = {
    config,
    pkgs,
    lib,
    ...
  }: let
    cfg = config.services.lspmux;
  in {
    options.services.lspmux = {
      enable = lib.mkEnableOption "the language server multiplexer server";

      package = lib.mkOption {
        type = lib.types.package;
        default = inputs.self.packages.${pkgs.stdenv.hostPlatform.system}.lspmux;
        defaultText = lib.literalExpression "inputs.self.packages.\${system}.lspmux";
        description = "The lspmux package to use.";
      };

      user = lib.mkOption {
        type = lib.types.str;
        description = "User under which the lspmux server runs.";
      };
    };

    config = lib.mkIf cfg.enable {
      systemd.services.lspmux = {
        description = "Language server multiplexer server";
        wantedBy = ["multi-user.target"];
        serviceConfig = {
          Type = "simple";
          ExecStart = "${cfg.package}/bin/lspmux server";
          User = cfg.user;
        };
      };
    };
  };
}
