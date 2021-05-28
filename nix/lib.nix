{ pkgs ? import <nixpkgs> { } }:

{

  easy-ps = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "a5fd0328827ac46954db08f624c09eba981f1ab2";
    sha256 = "1g3bk2y8hz0y998yixz3jmvh553kjpj2k7j0xrp4al1jrbdcmgjq";
  }) { inherit pkgs; };

  spago2nix = import (builtins.fetchGit {
    url = "git@github.com:justinwoo/spago2nix.git";
    rev = "a4622c3b27fca47e3276131a8ec4097a1d3c78a7";
  }) { inherit pkgs; };

}
