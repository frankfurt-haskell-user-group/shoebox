let
  bootstrap = import <nixpkgs> { };

  nixpkgs = builtins.fromJSON (builtins.readFile ./nixpkgs.json);

  src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    inherit (nixpkgs) rev sha256;
  };

  pkgs = import src { };

in
  pkgs.stdenv.mkDerivation  {
    name = "shoebox-frontend";
    buildInputs = with pkgs; [
      electron
      git
      haskellPackages.purescript
      less
      nodejs
      nodePackages.bower
      nodePackages.webpack
    ];
  }

