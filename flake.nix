{
  nixConfig = {
    extra-substituters = "https://cache.ners.ch/haskell";
    extra-trusted-public-keys = "haskell:WskuxROW5pPy83rt3ZXnff09gvnu80yovdeKDw5Gi3o=";
  };

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

  outputs = inputs:
    with builtins;
    let
      inherit (inputs.nixpkgs) lib;
      foreach = xs: f: with lib; foldr recursiveUpdate { } (
        if isList xs then map f xs
        else if isAttrs xs then mapAttrsToList f xs
        else throw "foreach: expected list or attrset but got ${typeOf xs}"
      );
      sourceFilter = root: with lib.fileset; toSource {
        inherit root;
        fileset = fileFilter
          (file: any file.hasExt [ "cabal" "hs" "md" "png" ])
          root;
      };
      ghcsFor = pkgs: with lib; foldlAttrs
        (acc: name: hp':
          let
            hp = tryEval hp';
            version = getVersion hp.value.ghc;
            majorMinor = versions.majorMinor version;
            ghcName = "ghc${replaceStrings ["."] [""] majorMinor}";
          in
          if hp.value ? ghc && ! acc ? ${ghcName} && versionAtLeast version "9.8" && versionOlder version "9.12"
          then acc // { ${ghcName} = hp.value; }
          else acc
        )
        { }
        pkgs.haskell.packages;
      hpsFor = pkgs: { default = pkgs.haskellPackages; } // ghcsFor pkgs;
      pnames = map (path: baseNameOf (dirOf path)) (lib.fileset.toList (lib.fileset.fileFilter (file: file.hasExt "cabal") ./.));
      libPnames = filter (pname: pname != "rhine-mine") pnames;
      haskell-overlay = final: prev: lib.composeManyExtensions [
        (hfinal: hprev: lib.genAttrs pnames (pname: hfinal.callCabal2nix pname (sourceFilter ./${pname}) { }))
        (hfinal: hprev: with prev.haskell.lib.compose; {
          lrucaching = doJailbreak (unmarkBroken hprev.lrucaching);
          rhine-sdl2 = hprev.rhine-sdl2.overrideAttrs (attrs: {
            meta.mainProgram = "example";
          });
          sdl2-image = lib.pipe hprev.sdl2-image [
            unmarkBroken
            (drv: drv.overrideAttrs (attrs: {
              strictDeps = true;
            }))
          ];
        })
      ];
      overlay = lib.composeManyExtensions [
        (final: prev: {
          haskell = prev.haskell // {
            packageOverrides = lib.composeManyExtensions [
              prev.haskell.packageOverrides
              (haskell-overlay final prev)
            ];
          };
        })
      ];
    in
    {
      overlays = {
        default = overlay;
        haskell = haskell-overlay;
      };
    }
    //
    foreach inputs.nixpkgs.legacyPackages
      (system: pkgs':
        let
          pkgs = pkgs'.extend overlay;
          hps = hpsFor pkgs;
          pname = "rhine-sdl2";
          bins = pkgs.buildEnv {
            name = "${pname}-bins";
            paths = map (pname: hps.default.${pname}) pnames;
            pathsToLink = [ "/bin" ];
          };
          libs = pkgs.buildEnv {
            name = "${pname}-libs";
            paths =
              lib.mapCartesianProduct
                ({ hp, pname }: hp.${pname})
                { hp = attrValues hps; pname = libPnames; };
            pathsToLink = [ "/lib" ];
          };
          docs = pkgs.buildEnv {
            name = "${pname}-docs";
            paths = map (pname: pkgs.haskell.lib.documentationTarball hps.default.${pname}) libPnames;
          };
          sdist = pkgs.buildEnv {
            name = "${pname}-sdist";
            paths = map (pname: pkgs.haskell.lib.sdistTarball hps.default.${pname}) libPnames;
          };
          docsAndSdist = pkgs.linkFarm "${pname}-docsAndSdist" { inherit docs sdist; };
        in
        {
          formatter.${system} = pkgs.nixpkgs-fmt;
          legacyPackages.${system} = pkgs;
          packages.${system}.default = pkgs.symlinkJoin {
            name = "${pname}-all";
            paths = [ bins libs docsAndSdist ];
            inherit (hps.default.rhine-sdl2) meta;
          };
          devShells.${system} =
            foreach hps (ghcName: hp: {
              ${ghcName} = hp.shellFor {
                packages = ps: map (pname: ps.${pname}) pnames;
                nativeBuildInputs = with pkgs'; with haskellPackages; [
                  cabal-install
                  cabal-gild
                  fourmolu
                ] ++ lib.optionals (lib.versionAtLeast (lib.getVersion hp.ghc) "9.4") [
                  hp.haskell-language-server
                ];
              };
            });
        }
      );
}
