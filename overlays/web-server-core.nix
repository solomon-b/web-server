# Exported overlay for external consumers of web-server-core.
# Note: In the local build, web-server-example.nix is applied second and
# replaces these overrides (Nix's `override` does not compose). This overlay
# still matters as `self.overlays.web-server-core` for downstream flakes.
self: final: prev: {
  haskellPackages = prev.haskellPackages.override {
    overrides = hfinal: hprev: {
      web-server-core = final.haskell.lib.dontCheck (
        hfinal.callCabal2nix "web-server-core" "${self}/web-server-core" { }
      );

      web-server-roles = final.haskell.lib.dontCheck (
        hfinal.callCabal2nix "web-server-roles" "${self}/web-server-roles" { }
      );
    };
  };
}
