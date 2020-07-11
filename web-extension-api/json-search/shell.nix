let
  pkgs = import <nixpkgs> {};
in
pkgs.mkShell {
  buildInputs = [
    pkgs.caddy
    pkgs.elmPackages.elm
  ];
}

