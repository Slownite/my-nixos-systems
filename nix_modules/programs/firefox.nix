{ config, lib, pkgs, ... }:

   let
    lock-false = {
      Value = false;
      Status = "locked";
    };
    lock-true = {
      Value = true;
      Status = "locked";
    };
  in
{

  options = {
      firefox.enable = lib.mkEnableOption "enable firefox";
  };
  config = lib.mkIf config.firefox.enable {

    programs = {
      firefox = {
        enable = true;
        languagePacks = [ "fr" "en-US" ];

        /* ---- POLICIES ---- */
        # Check about:policies#documentation for options.
        policies = {
          DisableTelemetry = true;
          DisableFirefoxStudies = true;
          EnableTrackingProtection = {
            Value= true;
            Locked = true;
            Cryptomining = true;
            Fingerprinting = true;
          };
          DisablePocket = true;
          DisableFirefoxAccounts = true;
          DisableAccounts = true;
          DisableFirefoxScreenshots = true;
          OverrideFirstRunPage = "";
          OverridePostUpdatePage = "";
          DontCheckDefaultBrowser = true;
          DisplayBookmarksToolbar = "never"; # alternatives: "always" or "newtab"
          DisplayMenuBar = "default-off"; # alternatives: "always", "never" or "default-on"
          SearchBar = "unified"; # alternative: "separate"

          /* ---- EXTENSIONS ---- */
          # Check about:support for extension/add-on ID strings.
          # Valid strings for installation_mode are "allowed", "blocked",
          # "force_installed" and "normal_installed".
          ExtensionSettings = {
            "*".installation_mode = "blocked"; # blocks all addons except the ones specified below
            # uBlock Origin:
            "uBlock0@raymondhill.net" = {
              install_url = "https://addons.mozilla.org/firefox/downloads/latest/ublock-origin/latest.xpi";
              installation_mode = "normal_installed";
            };
            # Privacy Badger:
            "jid1-MnnxcxisBPnSXQ@jetpack" = {
              install_url = "https://addons.mozilla.org/firefox/downloads/latest/privacy-badger17/latest.xpi";
              installation_mode = "normal_installed";
            };
            # Dark Reader:
            "addon@darkreader.org" = {
              install_url = "https://addons.mozilla.org/firefox/downloads/latest/dark-reader/latest.xpi";
              installation_mode = "normal_installed";
            };
            # Facebook Container:
            "@contain-facebook" = {
              install_url = "https://addons.mozilla.org/firefox/downloads/latest/facebook-container/latest.xpi";
              installation_mode = "normal_installed";
            };
            # Firefox Multi-Account Containers:
            "@testpilot-containers" = {
              install_url = "https://addons.mozilla.org/firefox/downloads/latest/firefox-multi-account-containers/latest.xpi";
              installation_mode = "normal_installed";
            };
            # Return YouTube Dislike:
            "{762f9885-5a13-4abd-9c77-433dcd38b8fd}" = {
              install_url = "https://addons.mozilla.org/firefox/downloads/latest/return-youtube-dislike/latest.xpi";
              installation_mode = "normal_installed";
            };
            # Scite:
            "{8bc17535-7be0-4000-825b-f87c7b45a65a}" = {
              install_url = "https://addons.mozilla.org/firefox/downloads/latest/scite/latest.xpi";
              installation_mode = "normal_installed";
            };
            # Side View:
            "side-view@mozilla.org" = {
              install_url = "https://addons.mozilla.org/firefox/downloads/latest/side-view/latest.xpi";
              installation_mode = "normal_installed";
            };
            # Dark Reader:
            "addon@darkreader.org" = {
              install_url = "https://addons.mozilla.org/firefox/downloads/latest/dark-reader/latest.xpi";
              installation_mode = "normal_installed";
            };
            # SponsorBlock for YouTube - Skip Sponsorships:
            "sponsorBlocker@ajay.app" = {
              install_url = "https://addons.mozilla.org/firefox/downloads/latest/sponsorBlock/latest.xpi";
              installation_mode = "normal_installed";
            };
            # Tab Stash:
            "tab-stash@condordes.net" = {
              install_url = "https://addons.mozilla.org/firefox/downloads/latest/tab-stash/latest.xpi";
              installation_mode = "normal_installed";
            };
          };

          /* ---- PREFERENCES ---- */
          # Check about:config for options.
          Preferences = {
            "browser.contentblocking.category" = { Value = "strict"; Status = "locked"; };
            "extensions.pocket.enabled" = lock-false;
            "extensions.screenshots.disabled" = lock-true;
            "browser.topsites.contile.enabled" = lock-false;
            "browser.formfill.enable" = lock-false;
            "browser.search.suggest.enabled" = lock-false;
            "browser.search.suggest.enabled.private" = lock-false;
            "browser.urlbar.suggest.searches" = lock-false;
            "browser.urlbar.showSearchSuggestionsFirst" = lock-false;
            "browser.newtabpage.activity-stream.feeds.section.topstories" = lock-false;
            "browser.newtabpage.activity-stream.feeds.snippets" = lock-false;
            "browser.newtabpage.activity-stream.section.highlights.includePocket" = lock-false;
            "browser.newtabpage.activity-stream.section.highlights.includeBookmarks" = lock-false;
            "browser.newtabpage.activity-stream.section.highlights.includeDownloads" = lock-false;
            "browser.newtabpage.activity-stream.section.highlights.includeVisited" = lock-false;
            "browser.newtabpage.activity-stream.showSponsored" = lock-true;
            "browser.newtabpage.activity-stream.system.showSponsored" = lock-true;
            "browser.newtabpage.activity-stream.showSponsoredTopSites" = lock-true;
          };
        };
      };
    };
  };
}
