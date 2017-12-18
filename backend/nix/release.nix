let
  bootstrap = import <nixpkgs> {};

  nixpkgs = builtins.fromJSON (builtins.readFile ./nixpkgs.json);

  src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    inherit (nixpkgs) rev sha256;
  };

  config = {
    allowUnfree = true;
  };

  pkgs = import src { inherit config; };

in
  {
    build = pkgs.haskellPackages.callPackage ./default.nix {};
  }

