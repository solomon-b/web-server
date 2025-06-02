final: prev: {
  haskellPackages = prev.haskellPackages.override {
    overrides = hfinal: hprev: {
      hasql = final.haskell.lib.dontCheck (final.haskell.lib.dontCheck (hfinal.callHackageDirect
        {
          pkg = "hasql";
          ver = "1.9.1.2";
          sha256 = "sha256-W2pAC3wLIizmbspWHeWDQqb5AROtwA8Ok+lfZtzTlQg=";
        }
        { }));

      hasql-pool = final.haskell.lib.dontCheck (final.haskell.lib.dontCheck (hfinal.callHackageDirect
        {
          pkg = "hasql-pool";
          ver = "1.3.0.2";
          sha256 = "sha256-3tADBDSR7MErgVLzIZdivVqyU99/A7jsRV3qUS7wWns=";
        }
        { }));

      hasql-transaction = final.haskell.lib.dontCheck (final.haskell.lib.dontCheck (hfinal.callHackageDirect
        {
          pkg = "hasql-transaction";
          ver = "1.2.0.1";
          sha256 = "sha256-gXLDMlD6E3degEUJOtFCiZf9EAsWEBJqsOfZK54iBSA=";
        }
        { }));

      web-server-core = final.haskell.lib.dontCheck (
        hfinal.callCabal2nix "web-server-core" ./web-server-core { }
      );

      text-builder = final.haskell.lib.dontCheck hfinal.text-builder_1_0_0_3;
    };
  };
}
