{ inputs }:

final: prev: {
  haskellPackages = prev.haskellPackages.override {
    overrides = hfinal: hprev: {
      hasql = final.haskell.lib.dontCheck final.haskellPackages.hasql_1_9_1_2;

      hasql-pool = final.haskell.lib.dontCheck final.haskellPackages.hasql-pool_1_3_0_1;

      # hasql-pool = final.haskell.lib.dontCheck (
      #   final.haskell.lib.dontCheck (
      #     hfinal.callHackageDirect {
      #       pkg = "hasql-pool";
      #       ver = "1.3.0.2";
      #       sha256 = "sha256-3tADBDSR7MErgVLzIZdivVqyU99/A7jsRV3qUS7wWns=";
      #     } { }
      #   )
      # );

      hasql-transaction = final.haskell.lib.dontCheck final.haskellPackages.hasql-transaction_1_2_0_1;

      hasql-interpolate = final.haskell.lib.dontCheck (
        hfinal.callCabal2nix "hasql-interpolate" "${inputs.hasql-interpolate-src}" { }
      );

      htmx = final.haskell.lib.dontCheck (
        hfinal.callHackageDirect {
          pkg = "htmx";
          ver = "0.1.0.0";
          sha256 = "sha256-RHpdjcqHBwA0u18h3TDNslhxsz0HXdy1pO5YYykc/jk=";
        } { }
      );

      text-builder = final.haskell.lib.dontCheck hfinal.text-builder_1_0_0_3;

      web-server-core = final.haskell.lib.dontCheck (
        hfinal.callCabal2nix "web-server-core" ../web-server-core { }
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
