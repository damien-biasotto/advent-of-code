{
  description = "Damo's aoc flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils }:
  utils.lib.eachDefaultSystem (system:
    let
      overlay = final: prev: {
	advent-of-code = final.callCabal2nix "advent-of-code" ./. {};
      };
      myHaskellPackages = pkgs.haskell.packages.ghc910.extend overlay;
      pkgs = import nixpkgs { inherit system; overlays = [overlay]; };
    in
      rec {

	packages.default = myHaskellPackages.advent-of-code;
	apps.default = {
	  type = "app";
	  program = "${packages.default}/bin/year2024";
	};
	  
	devShells.default = myHaskellPackages.shellFor {
	  packages = p: [
	    p.advent-of-code
	  ];
	  nativeBuildInputs = with myHaskellPackages; [
	    ghcid
	    cabal-install
	    haskell-language-server
	  ];

	  shellHook = ''
	  gen-hie > hie.yaml
	  '';
	};
      }
  );
}
