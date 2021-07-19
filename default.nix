let
  pkgs = import <nixpkgs> {};
  hp = pkgs.haskellPackages;
in with pkgs.haskell.lib; hp.developPackage {
  root = ./.;
  modifier = drv: drv // {
    envFunc = _: hp.shellFor {
      withHoogle = true;
      packages = p: [ drv ];
      buildInputs = with hp; [cabal-install ghcid hp.hpack brittany pkgs.daemon];
    };
  };
}

# TODO: run hoogle server automatically
