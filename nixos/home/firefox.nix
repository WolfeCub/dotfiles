_: {
  flake.homeModules.firefox = {pkgs, ...}: {
    home.packages = with pkgs; [
      firefox-devedition
    ];

    xdg.desktopEntries.firefox-private = {
      name = "Firefox Developer Edition (Private)";
      exec = "firefox-devedition --private-window %U";
      icon = "firefox-devedition";

      categories = ["Network" "WebBrowser"];
      genericName = "Web Browser";
      mimeType = [
        "text/html"
        "text/xml"
        "application/xhtml+xml"
        "application/vnd.mozilla.xul+xml"
        "x-scheme-handler/http"
        "x-scheme-handler/https"
      ];

      startupNotify = true;
      terminal = false;
      type = "Application";
    };
  };
}
