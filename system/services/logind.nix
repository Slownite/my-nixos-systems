{ config, lib, pkgs, ... }:

{
  services.logind = {
    lidSwitch = "ignore";
    lidSwitchDocked = "ignore";
    handleSuspendKey = "ignore";
    handleHibernateKey = "ignore";
    handleSuspendKeyDocked = "ignore";
    handleHibernateKeyDocked = "ignore";

  };
}
