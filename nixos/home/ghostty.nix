_: {
  flake.homeModules.ghostty = {dfRoot, ...}: {
    programs.ghostty = {
      enable = true;
      settings = {
        theme = "base16-default-dark";

        font-family = "FiraCode Nerd Font Mono";
        font-size = 11;

        mouse-hide-while-typing = true;
        window-decoration = "none";

        cursor-style = "block";
        cursor-style-blink = false;
        shell-integration-features = "no-cursor";
      };
    };

    xdg.configFile."ghostty/themes/base16-default-dark".source = dfRoot + /ghostty/.config/ghostty/themes/base16-default-dark;
  };
}
