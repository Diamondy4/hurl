{
  description = "hurl";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = inputs@{ self, flake-parts, nixpkgs, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.flake-parts.flakeModules.easyOverlay
      ];
      perSystem = { self', inputs', system, config, final, lib, pkgs, ... }:
        let
          curl' = pkgs.curl.override {
            openssl = pkgs.quictls;
            http3Support = true;
            c-aresSupport = true;
            brotliSupport = true;
            zstdSupport = true;
          };
          curlPath' = lib.makeLibraryPath [ pkgs.curl ];
        in
        {
          overlayAttrs = { curl = curl'; };
          haskellProjects.default = {
            devShell.tools = hp: {
              curl = final.curl;
            };
            overrides = hfinal: hprev:
              let
                hurl = hfinal.callCabal2nix "hurl" ./. {
                  uv = pkgs.libuv;
                  curl = final.curl;
                };
                hurlFlags = with pkgs.haskell.lib.compose; [
                  (appendConfigureFlag "--ghc-options=-lcurl")
                  (appendConfigureFlag "--ghc-options=-L${curlPath'}")
                  dontCheck
                  dontHaddock
                ];
              in
              { hurl = lib.pipe hurl hurlFlags; };
          };
          packages.default = self'.packages.hurl;
        };
    };
}
