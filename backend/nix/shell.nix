let
  pkgs = import <nixpkgs> {};
  buildEnv = (import ./release.nix).build.env;
  tools = with pkgs; [
    cabal-install
    cabal2nix
    git
    less
    nix-prefetch-git
  ];

in
  buildEnv.overrideAttrs (oldAttrs: {
    nativeBuildInputs = oldAttrs.nativeBuildInputs ++ tools;
  })

