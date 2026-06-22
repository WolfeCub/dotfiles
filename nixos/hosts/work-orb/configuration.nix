{inputs, ...}: {
  flake.nixosModules.vital-nix-orb = {
    config,
    pkgs,
    modulesPath,
    lib,
    ...
  }: {
    imports = with inputs.self.nixosModules; [
      # Include the default lxd configuration.
      "${modulesPath}/virtualisation/lxc-container.nix"
      workPostgres
      workRemoteBuild
      workSoftwareWorkstation
      lspmux
    ];

    users.users.wolfe = {
      # uid = 501;
      uid = 1000;
      extraGroups = ["wheel" "orbstack" "audio"];

      # simulate isNormalUser, but with an arbitrary UID
      # isSystemUser = true;
      # isNormalUser = lib.mkForce false; # Don't let user.nix override this

      isNormalUser = true;

      # group = "users";
      # createHome = true;
      # home = "/home/wolfe";
      # homeMode = "700";
      shell = pkgs.zsh;

      packages = with pkgs; [
        stow
        unstable.nodejs_latest
        lspmux
        unstable.claude-code
      ];
    };

    services.getty.autologinUser = "wolfe";
    programs.zsh.enable = true;
    security.sudo.wheelNeedsPassword = false;

    # This being `true` leads to a few nasty bugs, change at your own risk!
    users.mutableUsers = false;

    time.timeZone = "America/Toronto";

    networking = {
      dhcpcd.enable = false;
      useDHCP = false;
      useHostResolvConf = false;
    };

    systemd.network = {
      enable = true;
      networks."50-eth0" = {
        matchConfig.Name = "eth0";
        networkConfig = {
          DHCP = "ipv4";
          IPv6AcceptRA = true;
        };
        linkConfig.RequiredForOnline = "routable";
      };
    };

    networking.hostName = "vital-nix-orb";

    nix.settings.experimental-features = ["nix-command" "flakes"];
    programs.nix-ld.enable = true;

    # This option defines the first version of NixOS you have installed on this particular machine,
    # and is used to maintain compatibility with application data (e.g. databases) created on older NixOS versions.
    #
    # Most users should NEVER change this value after the initial install, for any reason,
    # even if you've upgraded your system to a new NixOS release.
    #
    # This value does NOT affect the Nixpkgs version your packages and OS are pulled from,
    # so changing it will NOT upgrade your system - see https://nixos.org/manual/nixos/stable/#sec-upgrading for how
    # to actually do that.
    #
    # This value being lower than the current NixOS release does NOT mean your system is
    # out of date, out of support, or vulnerable.
    #
    # Do NOT change this value unless you have manually inspected all the changes it would make to your configuration,
    # and migrated your data accordingly.
    #
    # For more information, see `man configuration.nix` or https://nixos.org/manual/nixos/stable/options#opt-system.stateVersion .
    system.stateVersion = "25.11"; # Did you read the comment?
  };
}
