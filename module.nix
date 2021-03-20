curve:
{ config, lib, pkgs, ... }:

let cfg = config.services.curve;
in {
  options.services.curve = {
    enable = lib.mkEnableOption "Curve Discord Bot";
    configFile = lib.mkOption {
      type = lib.types.path;
      default = false;
      description = ''
        Configuration file
      '';
    };
    environmentFile = lib.mkOption {
      type = lib.types.path;
      default = false;
      description = ''
        Environment file containing relevant variables
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    users.users.curve = {
      name = "curve";
      description = "Curve Discord Bot";
      home = "/var/empty";
      shell = null;
    };
  };
}
