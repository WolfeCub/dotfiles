{self, ...}: {
  flake.nixosModules.xdg-portal = {pkgs, ...}: {
    xdg.portal = {
      enable = true;
      extraPortals = [pkgs.xdg-desktop-portal-wlr];
      config.common.default = ["wlr"];

      wlr.settings.screencast = {
        chooser_type = "dmenu";
        chooser_cmd = "${self.packages.${pkgs.stdenv.hostPlatform.system}.noctalia}/bin/noctalia dmenu";
      };
    };
  };
}
