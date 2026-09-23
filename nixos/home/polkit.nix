_: {
  flake.homeModules.polkit = {...}: {
    services.polkit-gnome.enable = true;
  };
}
