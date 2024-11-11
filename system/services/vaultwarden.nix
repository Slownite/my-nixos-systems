{ config, lib, pkgs, ... }:
let
   cfg = config.vaultwarden;
in
{
  options.vaultwarden = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "enable the vaultwarden package";
    };
  };

  config = lib.mkIf cfg.enable {
    services.vaultwarden = { 
    enable = true;
    backupDir = "/mnt/nfs/backup/vaultwarden";
    };
  };
}
