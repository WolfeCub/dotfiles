_: {
  flake.nixosModules.workPostgres = {
    config,
    pkgs,
    lib,
    ...
  }:
  # Application Developer:
  let
    # Use `autologinUser` or fallback to the first user in the users list.
    userList = builtins.attrNames (
      lib.filterAttrs (name: user: user.isNormalUser or false) config.users.users
    );
    mainUser =
      if config.services.getty.autologinUser != null
      then config.services.getty.autologinUser
      else if userList != []
      then builtins.head userList
      else throw "No autologin user or normal users found for PostgreSQL setup";
  in {
    # Source: https://nixos.wiki/wiki/PostgreSQL
    config.services.postgresql = {
      enable = true;
      package = pkgs.postgresql_16;
      enableTCPIP = true;
      authentication = ''
        #type database DBuser origin-address auth-method
        # socket connections
        local all      all                   peer
        # Parallels VM shared subnet
        host  all      all    10.211.55.0/24 trust
      '';
      initialScript = pkgs.writeText "backend-initScript" ''
        CREATE ROLE ${mainUser} WITH LOGIN PASSWORD '${mainUser}' CREATEDB;
        CREATE DATABASE ${mainUser};
        GRANT ALL PRIVILEGES ON DATABASE ${mainUser} TO ${mainUser};
      '';
    };
  };
}
