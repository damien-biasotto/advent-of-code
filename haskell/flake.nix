{
  description = "Advent of Code - Haskell flavour";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils }: utils.lib.eachDefaultSystem (system:
    let
      overlay = pkgsNew: pkgsOld: {
	aoc = pkgsNew.haskell.lib.justStaticExecutables pkgsNew.haskellPackages.aoc;
	haskellPackages = pkgsOld.haskellPackages.override (old: {
	  overrides = pkgsNew.haskell.lib.packageSourceOverrides {
	    aoc = ./.;
	  };
	});
      };
      pkgs = import nixpkgs { inherit system; overlays = [ overlay ]; config.allowBroken = true; };
    in
      rec {
	packages.default = pkgs.haskellPackages.aoc;
	apps.default = {
	  type = "app";
	  program = "${pkgs.lib.getExe pkgs.aoc}";
	};
	
	devShells.default = pkgs.haskellPackages.aoc.env;
      }
  );
}
