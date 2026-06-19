{inputs, ...}: {
  flake.nixosModules.noctalia-greeter = {pkgs, ...}: {
    imports = [
      inputs.noctalia-greeter.nixosModules.default
    ];

    programs.noctalia-greeter = {
      enable = true;
      package = inputs.noctalia-greeter.packages.${pkgs.stdenv.hostPlatform.system}.default;
    };

    systemd.tmpfiles.rules = let
      greeterConfig = pkgs.writeText "greeter.conf" ''
        session=Mango
        scheme=Synced
        output="DP-3"
      '';
    in
      # L+ means "create a symlink and overwrite if it exists"
      [
        "L+ /var/lib/noctalia-greeter/greeter.conf - - - - ${greeterConfig}"
      ];
  };
}
