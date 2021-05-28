{ pkgs ? import <nixpkgs> { } }:

let
  pspkgs = import ./nix/nixed-packages.nix { inherit pkgs; };
  easy-ps = (import ./nix/lib.nix { inherit pkgs; }).easy-ps;
in

pkgs.stdenv.mkDerivation {
  name = "daygen";
  src = ./.;

  buildInputs = [
    pspkgs.installSpagoStyle
    pspkgs.buildSpagoStyle
  ];

  nativeBuildInputs = [
    easy-ps.purs-0_13_8
  ];

  unpackPhase = ''
    cp -r $src/{spago.dhall,packages.dhall,nix,src} .
    cp nix/nixed-packages.nix ./spago-packages.nix
    install-spago-style
  '';

  buildPhase = ''
    build-spago-style "./src/**/*.purs"
  '';

  installPhase = ''
    mkdir $out
    cp $src/index.html $out
    purs bundle "output/*/*.js" -m Main --main Main -o $out/index.js
  '';
}
