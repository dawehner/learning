{ pkgs ? import (fetchTarball https://git.io/Jf0cc) {} }:

let
  mach-nix = import (
    builtins.fetchGit {
      url = "https://github.com/DavHau/mach-nix/";
      ref = "refs/tags/3.1.1";
    }
  ) {};

  customPython = mach-nix.mkPython {
    requirements = ''
      scrapy
    '';
  };
in

pkgs.mkShell {
  buildInputs = [ customPython ];
}
