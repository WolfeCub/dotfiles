{pkgs, ...}: {
  users.users.wolfe = {
    shell = pkgs.zsh;
    isNormalUser = true;
    description = "Josh Wolfe";
    extraGroups = ["networkmanager" "wheel"];
  };

  programs.zsh.enable = true;
}
