{pkgs ? import <nixpkgs> {}}:

let 
  src = builtins.fetchGit {
    url = "https://gitlab.com/brestcc/nix";
    ref = "master"; 
  };
  altPkgs = import src pkgs; 
in 
pkgs.mkShell {
  buildInputs = with pkgs; 
    altPkgs.bundles.elm pkgs ++
    [ pkgs.nodePackages.uglify-js];
}
