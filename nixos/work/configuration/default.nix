{pkgs, ...}: {
  imports = [
    ./hardware-configuration.nix # Include the results of the hardware scan.
    ./software-workstation.nix
    ./postgres.nix
    ./remote-build.nix
  ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "vital-nix-vm"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "America/Toronto";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_CA.UTF-8";

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "dvp";
  };

  # Use the x11 keymap for ttys
  console.useXkbConfig = true;

  nix.settings.experimental-features = ["nix-command" "flakes"];

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.wolfe = {
    shell = pkgs.zsh;
    isNormalUser = true;
    description = "Josh Wolfe";
    extraGroups = ["networkmanager" "wheel"];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINnJSGHR4ANwejRjD/WVVtN366Fxf1XBv2KhH6mnfMPX wolfe@wolfe-vb-mbp"
    ];

    packages = with pkgs; [
      stow
      unstable.nodejs_latest
      customPkgs.lspmux
      unstable.claude-code
    ];
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    ghostty.terminfo
    rio.terminfo
  ];

  programs.zsh.enable = true;

  # Helps cargo find our private keys
  programs.ssh.startAgent = true;

  # If using VS Code. Allows the remote-ssh extension to link binaries as if on a non-nix system.
  programs.nix-ld.enable = true;

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "25.11"; # Did you read the comment?

  systemd.services.lspmux = {
    description = "Language server multiplexer server";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "simple";
      ExecStart = "${pkgs.customPkgs.lspmux}/bin/lspmux server";
      User = "wolfe";
    };
  };
}
