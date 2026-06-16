_: {
  flake.nixosModules.user = {
    pkgs,
    config,
    ...
  }: {
    users.users.wolfe = {
      shell = pkgs.zsh;
      isNormalUser = true;
      description = "Josh Wolfe";
      extraGroups = ["networkmanager" "wheel"];
    };

    programs.zsh.enable = true;

    # Unable sshAgent unless we've enabled gnome-keyring somewhere
    programs.ssh.startAgent = !config.services.gnome.gnome-keyring.enable;
  };
}
