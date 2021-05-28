{ pkgs ? import <nixpkgs> { } }:

let
  spago2nix = (import ./nix/lib.nix { inherit pkgs; }).spago2nix;
  easy-ps = (import ./nix/lib.nix { inherit pkgs; }).easy-ps;
in

pkgs.mkShell {
  buildInputs = [
    spago2nix
    easy-ps.purs-0_13_8
    easy-ps.spago
  ];
}
