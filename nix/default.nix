{ pkgs }:

let
  inherit (pkgs.lib) fold composeExtensions concatMap attrValues;

  hls = pkgs.haskell-language-server.override {
    supportedGhcVersions = [ "94" ];
  };

  combineOverrides = old:
    fold composeExtensions (old.overrides or (_: _: { }));

  testConfigurations =
    let
      makeTestConfiguration = { ghcVersion, overrides ? new: old: { } }:
        let
          inherit (pkgs.haskell.lib) dontCheck packageSourceOverrides;
        in
        (pkgs.haskell.packages.${ghcVersion}.override (old: {
          overrides =
            combineOverrides old [
              (packageSourceOverrides { cookie-tray = ../cookie-tray; })
              overrides
            ];
        })).cookie-tray;
    in
    rec {
      ghc-9-2 = makeTestConfiguration {
        ghcVersion = "ghc92";
      };
      ghc-9-4 = makeTestConfiguration {
        ghcVersion = "ghc94";
      };
      ghc-9-6 = makeTestConfiguration {
        ghcVersion = "ghc96";
        overrides = new: old: {
          # x = new.callPackage ./haskell/x.nix { };
        };
      };
      all = pkgs.symlinkJoin {
        name = "cookie-tray-tests";
        paths = [ ghc-9-2 ghc-9-4 ghc-9-6 ];
      };
    };

in
{

  packages = { inherit testConfigurations; };

  devShells.default = pkgs.mkShell {
    inputsFrom = [ testConfigurations.ghc-9-4.env ];
    buildInputs = [
      hls
      pkgs.cabal-install
      pkgs.ormolu
    ];
  };

}
