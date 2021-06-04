{
  description = "OPC XML-DA Client";

  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        compiler = "ghc8104";

        overlays = [
          haskellNix.overlay
          (final: prev: {
            opc-xml-da-client-project =
              final.haskell-nix.project' {
                src = ./.;
                compiler-nix-name = compiler;
                projectFileName = "cabal.project";
              };
          })
        ];

        pkgs = import nixpkgs { inherit system overlays; };
        flake = pkgs.opc-xml-da-client-project.flake { };

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

      in
      flake // {
        defaultPackage = flake.packages."opc-xml-da-client:lib:opc-xml-da-client";

        devShell = pkgs.opc-xml-da-client-project.shellFor {
          tools = {
            cabal = "latest";
            hlint = "latest";
            haskell-language-server = "latest";
          };
        };
      });
}
