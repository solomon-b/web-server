self: final: prev: {
  haskellPackages = prev.haskellPackages.override {
    overrides = hfinal: hprev: {
      hasql = final.haskell.lib.dontCheck final.haskellPackages.hasql_1_9_1_2;
      hasql-pool = final.haskell.lib.dontCheck final.haskellPackages.hasql-pool_1_3_0_1;
      hasql-transaction = final.haskell.lib.dontCheck final.haskellPackages.hasql-transaction_1_2_0_1;

      # hasql-pool = final.haskell.lib.dontCheck (
      #   final.haskell.lib.dontCheck (
      #     hfinal.callHackageDirect {
      #       pkg = "hasql-pool";
      #       ver = "1.3.0.2";
      #       sha256 = "sha256-3tADBDSR7MErgVLzIZdivVqyU99/A7jsRV3qUS7wWns=";
      #     } { }
      #   )
      # );

      web-server-core = final.haskell.lib.dontCheck (
        hfinal.callCabal2nix "web-server-core" "${self}/web-server-core" { }
      );

      text-builder = final.haskell.lib.dontCheck hfinal.text-builder_1_0_0_3;
    };
  };
}
