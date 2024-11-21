{
  description = "Advent of Code - Haskell flavour";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils }: utils.lib.eachDefaultSystem (system:
    let
      overlay = pkgsNew: pkgsOld: {
	advent-of-code = pkgsNew.haskell.lib.justStaticExecutables pkgsNew.haskellPackages.advent-of-code;
	haskellPackages = pkgsOld.haskellPackages.override (old: {
	  overrides = pkgsNew.haskell.lib.packageSourceOverrides {
	    advent-of-code = ./.;
	  };
	});
      };
      pkgs = import nixpkgs { inherit system; overlays = [ overlay ]; config.allowBroken = true; };
    in
      rec {
	packages.default = pkgs.haskellPackages.advent-of-code;
	apps.default = {
	  type = "app";
	  program = "${pkgs.lib.getExe pkgs.advent-of-code}";
	};
	
	devShells.default = pkgs.haskellPackages.advent-of-code.env;
      }
  );
}
