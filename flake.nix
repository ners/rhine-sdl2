{
  nixConfig = {
    extra-substituters = "https://cache.ners.ch/haskell";
    extra-trusted-public-keys = "haskell:WskuxROW5pPy83rt3ZXnff09gvnu80yovdeKDw5Gi3o=";
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nix-filter.url = "github:numtide/nix-filter";
    dunai = {
      url = "github:ivanperez-keera/dunai";
      flake = false;
    };
    dunai-transformers = {
      url = "github:ghc/packages-transformers";
      flake = false;
    };
    rhine = {
      url = "github:turion/rhine";
      flake = false;
    };
  };

  outputs = inputs:
    with builtins;
    let
      inherit (inputs.nixpkgs) lib;
      foreach = xs: f: with lib; foldr recursiveUpdate { } (
        if isList xs then map f xs
        else if isAttrs xs then mapAttrsToList f xs
        else throw "foreach: expected list or attrset but got ${typeOf xs}"
      );
      hsSrc = root: inputs.nix-filter {
        inherit root;
        include = with inputs.nix-filter.lib; [
          (matchExt "cabal")
          (matchExt "hs")
          (matchExt "md")
          isDirectory
        ];
      };
      pname = "rhine-sdl2";
      ghcs = [ "ghc92" "ghc94" ];
      hpsFor = pkgs:
        lib.filterAttrs (ghc: _: elem ghc ghcs) pkgs.haskell.packages
        // { default = pkgs.haskell.packages.ghc94; };
      overlay = final: prev: lib.pipe prev [
        (prev: {
          haskell = prev.haskell // {
            packageOverrides = with prev.haskell.lib.compose; lib.composeManyExtensions [
              prev.haskell.packageOverrides
              (hfinal: hprev: {
                dunai = hfinal.callCabal2nix "dunai" "${inputs.dunai}/dunai" {
                  transformers = hprev.callCabal2nix "transformers" inputs.dunai-transformers { };
                };
                rhine = doJailbreak (hfinal.callCabal2nix "rhine" "${inputs.rhine}/rhine" { });
                ${pname} = (hfinal.callCabal2nix pname (hsSrc ./.) { }).overrideAttrs (attrs: {
                  meta.mainProgram = "example";
                });
              })
            ];
          };
        })
      ];
    in
    {
      overlays.default = overlay;
    }
    //
    foreach inputs.nixpkgs.legacyPackages
      (system: pkgs':
        let pkgs = pkgs'.extend overlay; in
        {
          formatter.${system} = pkgs.nixpkgs-fmt;
          legacyPackages.${system} = pkgs;
          packages.${system}.default = (hpsFor pkgs).default.${pname};
          devShells.${system} =
            foreach (hpsFor pkgs) (ghcName: hp: {
              ${ghcName} = hp.shellFor {
                packages = ps: [ ps.${pname} ];
                nativeBuildInputs = with hp; [
                  pkgs'.haskellPackages.cabal-install
                  pkgs'.haskellPackages.fourmolu
                  haskell-language-server
                ];
              };
            });
        }
      );
}
