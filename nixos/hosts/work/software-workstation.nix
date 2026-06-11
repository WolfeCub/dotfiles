_: {
  flake.nixosModules.vital-nix-vm = {pkgs, ...}: {
    # Software Workstation:

    # imports = [
    #   (fetchTarball "https://github.com/nix-community/nixos-vscode-server/tarball/master")
    # ];
    config = {
      boot.kernelParams = ["usbcore.usbfs_memory_mb=1000"];

      programs = {
        ssh.startAgent = true;
        direnv.enable = true;
      };

      environment = {
        systemPackages = with pkgs; [
          vim
          wget
          gitFull
          curl
        ];
      };

      networking.firewall.enable = false;

      # system.copySystemConfiguration = true;

      security.pki.certificates = [
        ''
          Vital internal-ca
          -----BEGIN CERTIFICATE-----
          MIIDXTCCAkWgAwIBAgIUFsse2UrbIr/8jaYFAgIYXz6Fse0wDQYJKoZIhvcNAQEL
          BQAwHDEaMBgGA1UEAwwRVml0YWwgSW50ZXJuYWwgQ0EwHhcNMjAxMDE2MjAwNDA1
          WhcNMzAxMDE0MjAwNDA1WjAcMRowGAYDVQQDDBFWaXRhbCBJbnRlcm5hbCBDQTCC
          ASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAL9ZbKzMJW3vz56maKXveIMS
          8DW5BWCpAzg7I4p/Dym8eFyc4ayRdHS5IN3tNYik1KyhsyGq9Jxf6JCjq/ngWlKP
          BSjFwF00U+cl7wjjxtT4H9DlStJqdSwmPUfwRJwWf054gsEBTqizrXw3rPr9s7Jb
          qHem3yfy7hWwSMyR+iD0de1/5rFT5shpdSNLp7HjCuBKfmpD9H+3+EMMQtivvOSe
          8iGe4J5Bw0uMMyB8MHGGwvWUaJoptxVBIRlH/MPUSshRteiZgGZsl/U/314rVVBo
          X/904xCG2s2351eQ8+3IgHNF5e5lR2MvGaF/ipLYgKIqxW7F413F/SxEhGdEmhkC
          AwEAAaOBljCBkzAdBgNVHQ4EFgQUXKKHsSmO4D6AW0iTzcFsBjOVkr4wVwYDVR0j
          BFAwToAUXKKHsSmO4D6AW0iTzcFsBjOVkr6hIKQeMBwxGjAYBgNVBAMMEVZpdGFs
          IEludGVybmFsIENBghQWyx7ZStsiv/yNpgUCAhhfPoWx7TAMBgNVHRMEBTADAQH/
          MAsGA1UdDwQEAwIBBjANBgkqhkiG9w0BAQsFAAOCAQEAefNzUvPPJwAVL6eO1nrl
          ceQ5H7XRp493RNANyKaJ8lsrCC8zj0M5OFv7RG0fqc2jjjMZek13r9AACO9K9ZK5
          yTFC2D+o59hAqNuU7m1vGMC0fxyWU0XuFTtJr+p7zoMchP32ZplDazMAJucoznYd
          EQJxS4yJet0SHMZPfN4b1A+iGexTiu7RRdQvlW7fNec41a8K9C6hpVZSkS4zX8hS
          dm8itppN5w1IpYqIl4kMXo2+RaHImwzjcusuHGYyOkSltWt9xop5r7fcuYVXYnRM
          ZmcVAzC4tY8+xgKiHTyRMWcuqbf+ZYXv11myUf/bHWQ0Swp8KJRjIBrW2ZQd9bPX
          fA==
          -----END CERTIFICATE-----
        ''
      ];

      services = {
        openssh.enable = true;
        lorri.enable = true;
        # vscode-server.enable = true;
      };

      nix = {
        settings = {
          substituters = [
            "https://cache.nixos.org/"
            "https://hydra.vital.company/"
          ];
          trusted-public-keys = ["hydra.vital.company-1:olecgNoiYwSyPA3/vfE7bbkq0yfp5NGbV1xdc/LZpIQ="];
        };
        # extraOptions = ''
        #   extra-experimental-features = nix-command flakes
        #   netrc-file = /etc/netrc
        # '';
        extraOptions = ''
          netrc-file = /etc/netrc
        '';
      };
    };
  };
}
