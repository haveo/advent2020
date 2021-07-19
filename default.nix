let
  pkgs = import <nixpkgs> {};
  hp = pkgs.haskellPackages;
in with pkgs.haskell.lib; hp.developPackage {
  root = ./.;
  modifier = drv:
    let
      addHook = d: overrideCabal d (_: {
        shellHook =
          ''
            daemon --name=hoogle -- hoogle server --local
            function finish {
              daemon -n hoogle --signal SIGTERM;
            }
            trap finish EXIT
          '';
      });
      addTools = d:
        addBuildTools d
          (with hp; [cabal-install ghcid brittany pkgs.daemon hp.hpack]);
    in addTools (addHook drv);
}
