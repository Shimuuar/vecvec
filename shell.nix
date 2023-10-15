let

  srcs = builtins.fetchGit {
    url = "https://github.com/Shimuuar/vecvec.git";
    rev = "d20ee2a555ead8062bc117b3c6a5affb17758671";
  };

  myHaskellPackageOverlay = self: super: {
    myHaskellPackages = super.haskell.packages.ghc943.override {
      overrides = hself: hsuper: rec {
        vecvec-classes = hself.callCabal2nixWithOptions "vecvec-classes" srcs "--subpath vecvec-classes" { };
        vecvec-hmatrix = hself.callCabal2nixWithOptions "vecvec-hmatrix" srcs "--subpath vecvec-hmatrix" { };
        vecvec-lapack = hself.callCabal2nixWithOptions "vecvec-lapack" srcs "--subpath vecvec-lapack" { };
        vecvec-lapack-ffi = hself.callCabal2nixWithOptions "vecvec-lapack-ffi" srcs "--subpath vecvec-lapack-ffi"
          { lapacke = super.lapack; };
        vecvec-lorentz = super.haskell.lib.dontCheck (
          hself.callCabal2nixWithOptions "vecvec-lorentz" srcs "--subpath vecvec-lorentz" { }
        );
        vector = super.haskell.lib.dontCheck (hself.callHackage "vector" "0.13.0.0" { });
        doctest = hself.callCabal2nixWithOptions "doctest" (builtins.fetchGit {
          url = "https://github.com/sol/doctest.git";
          rev = "95db1e1c01becd5b784624e466774863b605b7e3";
        }) "" { };
        http-api-data = super.haskell.lib.doJailbreak (hsuper.callHackage "http-api-data" "0.4.3" {});
        string-qq = super.haskell.lib.doJailbreak hsuper.string-qq;
        vector-algorithms = super.haskell.lib.doJailbreak (hsuper.callHackage "vector-algorithms" "0.9.0.1" {});
        servant = super.haskell.lib.doJailbreak hsuper.servant;
        JuicyPixels = super.haskell.lib.doJailbreak hsuper.JuicyPixels;
        skylighting-core = super.haskell.lib.dontCheck hsuper.skylighting-core;
        servant-server = super.haskell.lib.dontCheck hsuper.servant-server;
      };
    };
  };

  pkgs   = import <nixpkgs> { inherit config overlays; };
  config = { allowBroken = true; };
  overlays = [ myHaskellPackageOverlay ];

  pkgs_hask = pkgs.myHaskellPackages.ghcWithPackages (p: with p; [
    hmatrix
    vector
    fixed-vector
    vecvec-classes
    vecvec-hmatrix
    vecvec-lapack
    vecvec-lapack-ffi
    vecvec-lorentz
  ]);
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    openblasCompat
    lapack
    myHaskellPackages.BlogLiterately
    pkgs_hask
  ];
  shellHook = ''
    export LD_LIBRARY_PATH=${pkgs.openblasCompat}/lib''${LD_LIBRARY_PATH:+:}$LD_LIBRARY_PATH
    '';
}
