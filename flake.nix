{
  description = "Haskell curl bindings";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };

  outputs = inputs @ {
    self,
    flake-parts,
    nixpkgs,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
      ];
      perSystem = {
        self',
        inputs',
        system,
        config,
        final,
        lib,
        pkgs,
        ...
      }: let
        curl' = pkgs.curlHTTP3.override {
          c-aresSupport = true;
          brotliSupport = true;
          zstdSupport = true;
        };
      in {
        _module.args.pkgs = import self.inputs.nixpkgs {
          inherit system;
          overlays = [
            (final: prev: {
              uv = prev.libuv;
              libcurl = curl';
            })
          ];
        };

        haskellProjects.default = {
          autoWire = ["packages" "checks"];
          settings = {
            hurl = {self, ...}: {
              extraConfigureFlags = [
                "--ghc-options=-lcurl"
                "--ghc-options=-L${lib.makeLibraryPath [curl']}"
              ];
              custom = prev:
                pkgs.haskell.lib.overrideCabal prev (drv: {
                  libraryToolDepends = (drv.libraryToolDepends or []) ++ [self.c2hs];
                });
            };
          };
        };
        packages.default = self'.packages.hurl;
        devShells.default = pkgs.mkShell {
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
          ];
          packages = with pkgs; [
            uv
            libcurl
            libcurl.dev
          ];
          nativeBuildInputs = with pkgs; [
            pkg-config
          ];
        };
      };
    };
}
