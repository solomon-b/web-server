{
  description = "kpbj.fm";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixpkgs-unstable;
    flake-utils.url = github:numtide/flake-utils;

    cfg-src = {
      url = github:JonathanLorimer/cfg;
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, cfg-src }:
    let
      ghcVersion = "963";
      compiler = "ghc${ghcVersion}";
    in
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = import nixpkgs { inherit system; };
          hsPkgs = pkgs.haskell.packages.${compiler}.override {
            overrides = hfinal: hprev: {
              hasql-pool = pkgs.haskell.lib.dontCheck hprev.hasql-pool_1_1;
              web-server = hfinal.callCabal2nix "web-server" ./. { };
              cfg = hfinal.callCabal2nix "cfg" "${cfg-src}" { };
              rel8 = pkgs.haskell.lib.dontCheck hprev.rel8;
              servant-auth-server = pkgs.haskell.lib.markUnbroken (pkgs.haskell.lib.dontCheck hprev.servant-auth-server);
            };
          };
        in
        rec {
          devShell = pkgs.mkShell {
            buildInputs = [
              pkgs.cabal-install
              pkgs.flyctl
              pkgs.haskell.compiler.${compiler}
              pkgs.haskell.packages.${compiler}.haskell-language-server
              pkgs.haskell.packages.${compiler}.hlint
              pkgs.just
              pkgs.nixpkgs-fmt
              pkgs.ormolu
              pkgs.openssl
              pkgs.postgresql
              pkgs.pkg-config
              pkgs.shellcheck
              pkgs.sqlx-cli
              pkgs.zlib
              pkgs.zlib.dev
            ];
            LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [
              pkgs.cabal-install
              pkgs.flyctl
              pkgs.haskell.compiler.${compiler}
              pkgs.haskell.packages.${compiler}.haskell-language-server
              pkgs.haskell.packages.${compiler}.hlint
              pkgs.just
              pkgs.nixpkgs-fmt
              pkgs.ormolu
              pkgs.openssl
              pkgs.postgresql
              pkgs.pkg-config
              pkgs.shellcheck
              pkgs.sqlx-cli
              pkgs.zlib
              pkgs.zlib.dev
            ];
          };

          formatter = pkgs.nixpkgs-fmt;
          packages = flake-utils.lib.flattenTree {
            web-server = hsPkgs.web-server;
          };

          defaultPackage = packages.web-server;

          apps = {
            web-server = {
              type = "app";
              program = "${self.packages.${system}.webserver-backend}/bin/web-server";
            };

            default = self.apps.${system}.web-server;
          };
        });
}
