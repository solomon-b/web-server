{
  description = "kpbj.fm";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixpkgs-unstable;
    flake-utils.url = github:numtide/flake-utils;

    cfg-src = {
      url = github:JonathanLorimer/cfg;
      flake = false;
    };

    hasql-interpolate-src = {
      url = "github:awkward-squad/hasql-interpolate";
      flake = false;
    };
    hasql-src = {
      url = "github:JonathanLorimer/hasql/expose-hasql-encoders-params";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, cfg-src, hasql-interpolate-src, hasql-src }:
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
              hasql = pkgs.haskell.lib.dontCheck (
                hfinal.callCabal2nix
                  "hasql"
                  "${hasql-src}"
                  { }
              );

              hasql-pool = pkgs.haskell.lib.dontCheck (hfinal.callHackageDirect
                {
                  pkg = "hasql-pool";
                  ver = "1.2.0.2";
                  sha256 = "sha256-YBPHmeIb8PlmjtsHCjsRzH+H/1V8UjA1U2Fw48b8a4w=";
                }
                { });

              hasql-transaction = pkgs.haskell.lib.dontCheck (hfinal.callHackageDirect
                {
                  pkg = "hasql-transaction";
                  ver = "1.1.1.2";
                  sha256 = "sha256-MDP41k6QlRHoHUY1pT3fCQyvjS6XkVUlMge6NvxsluM=";
                }
                { });

              # External Packages from source
              hasql-interpolate = pkgs.haskell.lib.dontCheck (
                hfinal.callCabal2nix
                  "hasql-interpolate"
                  "${hasql-interpolate-src}"
                  { }
              );

              postgresql-binary = pkgs.haskell.lib.dontCheck (hfinal.callHackageDirect
                {
                  pkg = "postgresql-binary";
                  ver = "0.14";
                  sha256 = "sha256-ldXhe3JpdOXQvB7LE+5D4SUpNuwRjfw7zceV9BTVcUA=";
                }
                { });
              postgresql-libpq = pkgs.haskell.lib.dontCheck (hfinal.callHackageDirect
                {
                  pkg = "postgresql-libpq";
                  ver = "0.10.1.0";
                  sha256 = "sha256-tXOMqCO8opMilI9rx0D+njqjIjbZsH168Bzb8Aq8Ff4=";
                }
                { });

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
