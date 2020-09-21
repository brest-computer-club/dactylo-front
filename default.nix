let 
  pkgs = import (builtins.fetchTarball {
    name = "nixos-20.03";
    url = "https://github.com/NixOS/nixpkgs/archive/20.03.tar.gz";
    sha256 = "0182ys095dfx02vl2a20j1hz92dx3mfgz2a6fhn31bqlp1wa8hlq";
  }) {};
  src = builtins.fetchGit {
    url = "https://github.com/brest-computer-club/nix";
    ref = "master"; 
  };
  altPkgs = import src pkgs; 
in 
pkgs.mkShell {
  buildInputs = with pkgs; 
    altPkgs.bundles.elm pkgs ++
    [ pkgs.nodePackages.uglify-js];
}
