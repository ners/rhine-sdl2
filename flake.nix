{
  nixConfig = {
    extra-substituters = "https://cache.ners.ch/haskell";
    extra-trusted-public-keys = "haskell:WskuxROW5pPy83rt3ZXnff09gvnu80yovdeKDw5Gi3o=";
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    rhine = {
      url = "github:turion/rhine";
      flake = false;
    };
  };

  outputs =
    inputs:
      with builtins;
      let
        inherit (inputs.nixpkgs) lib;
        foreach = xs: f: with lib; foldr recursiveUpdate { } (
          if isList xs then map f xs
          else if isAttrs xs then mapAttrsToList f xs
          else throw "foreach: expected list or attrset but got ${typeOf xs}"
        );
        readDirs = root: attrNames (lib.filterAttrs (_: type: type == "directory") (readDir root));
        readFiles = root: attrNames (lib.filterAttrs (_: type: type == "regular") (readDir root));
        basename = path: suffix: with lib; pipe path [
          (splitString "/")
          last
          (removeSuffix suffix)
        ];
        cabalProjectPackages = root: with lib; foreach (readDirs root) (dir:
          let
            path = "${root}/${dir}";
            files = readFiles path;
            cabalFiles = filter (strings.hasSuffix ".cabal") files;
            pnames = map (path: basename path ".cabal") cabalFiles;
            pname = if pnames == [ ] then null else head pnames;
          in
          optionalAttrs (pname != null) { ${pname} = path; }
        );
        cabalProjectPnames = root: lib.attrNames (cabalProjectPackages root);
        cabalProjectOverlay = root: hfinal: hprev: with lib;
          mapAttrs (pname: path: hfinal.callCabal2nix pname path { }) (cabalProjectPackages root);
        pnames = cabalProjectPnames ./.;
        pickPnames = hp: map (pname: hp.${pname}) pnames;
        hpsFor = pkgs: with lib;
          { default = pkgs.haskellPackages; }
          // filterAttrs
            (name: hp: match "ghc[0-9]{2}" name != null && versionAtLeast hp.ghc.version "9.2")
            pkgs.haskell.packages;
        overlay = lib.composeManyExtensions [
          (final: prev: {
            haskell = prev.haskell // {
              packageOverrides = lib.composeManyExtensions [
                prev.haskell.packageOverrides
                (cabalProjectOverlay ./.)
                (cabalProjectOverlay inputs.rhine)
                (
                  hfinal: hprev: with prev.haskell.lib.compose; {
                    lrucaching = markUnbroken hprev.lrucaching;
                    rhine = doJailbreak (dontCheck hprev.rhine);
                    rhine-sdl2 = hprev.rhine-sdl2.overrideAttrs (attrs: {
                      meta.mainProgram = "example";
                    });
                    sdl2 = doJailbreak hprev.sdl2;
                    sdl2-image = lib.pipe hprev.sdl2-image [
                      markUnbroken
                      (drv:
                        drv.overrideAttrs (attrs: {
                          strictDeps = true;
                        })
                      )
                    ];
                    time-domain = doJailbreak hprev.time-domain;
                  }
                )
              ];
            };
          })
          (final: prev: {
            rhine-sdl2-all =
              let hps = hpsFor final; in
              final.buildEnv {
                name = "rhine-sdl2-all";
                paths = lib.pipe hps [
                  attrValues
                  (map pickPnames)
                  concatLists
                ];
                pathsToLink = [ "/lib" ];
                postBuild = ''
                  mkdir -p $out/bin
                  ln -s {${concatStringsSep "," (pickPnames hps.default)}}/bin/* $out/bin
                '';
                meta.mainProgram = hps.default.rhine-sdl2.meta.mainProgram;
              };
          })
          (final: prev: lib.optionalAttrs (prev.stdenv.hostPlatform.isWindows) { })
        ];
      in
      {
        overlays.default = overlay;
      }
      //
      foreach inputs.nixpkgs.legacyPackages (system: pkgs':
      let pkgs = pkgs'.extend overlay; in
      {
        formatter.${system} = pkgs.nixpkgs-fmt;
        legacyPackages.${system} = pkgs;
        packages.${system} =
          { default = pkgs.rhine-sdl2-all; }
          // foreach pnames (pname: {
            ${pname} = pkgs.haskellPackages.${pname};
          });
        devShells.${system} = foreach (hpsFor pkgs) (ghcName: hp: {
          ${ghcName} = hp.shellFor {
            packages = ps: map (pname: ps.${pname}) pnames;
            nativeBuildInputs = with hp; [
              pkgs'.haskellPackages.cabal-install
              pkgs'.haskellPackages.fourmolu
              haskell-language-server
            ];
          };
        });
      });
}
