let
  pkgs   = import <nixpkgs> { inherit config overlays; };
  config = {};
  overlays = [];

  pkgs_hask = pkgs.haskell.packages.ghc943.ghcWithPackages (p: with p; [
    hmatrix
    vector
    fixed-vector
  ]);
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    openblasCompat
    lapack
    pkg-config
#    pkgs_hask
  ];
  shellHook = ''
    export LD_LIBRARY_PATH=${pkgs.openblasCompat}/lib''${LD_LIBRARY_PATH:+:}$LD_LIBRARY_PATH
    '';
}
