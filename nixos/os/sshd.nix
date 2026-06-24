_: {
  flake.nixosModules.sshd = {pkgs, ...}: {
    services.openssh.enable = true;
  };
}
