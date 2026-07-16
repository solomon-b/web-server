{ inputs }:

final: prev: {
  haskellPackages = prev.haskellPackages.override {
    overrides = hfinal: hprev: {
      # These pin hs-opentelemetry-api ==0.2.* but nixpkgs 25.11 ships 0.3.0.0.
      # They compile fine against 0.3; the bounds are just overly tight upstream.
      hs-opentelemetry-exporter-handle = final.haskell.lib.markUnbroken (
        final.haskell.lib.doJailbreak hprev.hs-opentelemetry-exporter-handle
      );

      hs-opentelemetry-instrumentation-wai = final.haskell.lib.markUnbroken (
        final.haskell.lib.doJailbreak hprev.hs-opentelemetry-instrumentation-wai
      );

      htmx = final.haskell.lib.dontCheck (
        hfinal.callHackageDirect {
          pkg = "htmx";
          ver = "0.1.0.0";
          sha256 = "sha256-RHpdjcqHBwA0u18h3TDNslhxsz0HXdy1pO5YYykc/jk=";
        } { }
      );

      web-server-core = final.haskell.lib.dontCheck (
        hfinal.callCabal2nix "web-server-core" ../web-server-core { }
      );

      web-server-otel = final.haskell.lib.dontCheck (
        hfinal.callCabal2nix "web-server-otel" ../web-server-otel { }
      );

      servant-auth-roles = final.haskell.lib.dontCheck (
        hfinal.callCabal2nix "servant-auth-roles" ../servant-auth-roles { }
      );

      web-server-example = final.haskell.lib.dontCheck (
        hfinal.callCabal2nix "web-server-example" ../example { }
      );

      xmlhtml-lens = final.haskell.lib.dontCheck (hfinal.callCabal2nix "web-server" ../xmlhtml-lens { });

      xmlhtml-qq = final.haskell.lib.dontCheck (hfinal.callCabal2nix "web-server" ../xmlhtml-qq { });

      tmp-postgres = final.haskell.lib.dontCheck (
        hfinal.callCabal2nix "tmp-postgres" inputs.tmp-postgres-src { }
      );
    };
  };
}
