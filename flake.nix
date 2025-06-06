{
  description = "web-server";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixpkgs-unstable;
    flake-utils.url = github:numtide/flake-utils;

    hasql-interpolate-src = {
      url = github:awkward-squad/hasql-interpolate;
      flake = false;
    };
    hasql-src = {
      url = github:JonathanLorimer/hasql/expose-hasql-encoders-params;
      flake = false;
    };

    tmp-postgres-src = {
      url = github:jfischoff/tmp-postgres;
      flake = false;
    };
  };

  outputs = inputs@{ self, nixpkgs, flake-utils, hasql-interpolate-src, hasql-src, tmp-postgres-src }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ]
      (system:
        let
          pkgs = import nixpkgs {
            inherit system;
            config.allowUnfree = true;
            overlays = [
              (import ./overlays/web-server-core.nix)
              (import ./overlays/server.nix { inherit inputs; })
            ];
          };
          hsPkgs = pkgs.haskellPackages;
        in
        rec {
          devShell = pkgs.mkShell {
            buildInputs = [
              pkgs.cabal-install
              pkgs.flyctl
              pkgs.haskellPackages.ghc
              pkgs.haskellPackages.haskell-language-server
              pkgs.haskellPackages.hlint
              pkgs.just
              pkgs.nixpkgs-fmt
              pkgs.ngrok
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
              pkgs.haskellPackages.ghc
              pkgs.haskellPackages.haskell-language-server
              pkgs.haskellPackages.hlint
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

          packages = flake-utils.lib.flattenTree rec {
            web-server = hsPkgs.web-server;
            web-server-core = hsPkgs.web-server-core;
            ngrok-runner = pkgs.writeShellScriptBin "ngrok-runner.sh" ''
              ${pkgs.ngrok}/bin/ngrok http http://localhost:2000

            '';
            default = hsPkgs.web-server;
          };

          apps = {
            web-server = flake-utils.lib.mkApp { drv = self.packages.${system}.web-server; };
            ngrok = flake-utils.lib.mkApp { drv = self.packages.${system}.ngrok-runner; };
            default = self.apps.${system}.web-server;
          };
        }) // {
      overlays.web-server-core = import ./overlay.nix;
    };
}
