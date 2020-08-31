{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
    buildInputs = [ pkgs.antlr4 ];
}
