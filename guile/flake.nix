{
  description = "AOC 2024 - Guile Edition";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils }:
  utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
    in {
      devShell = pkgs.mkShell {
	buildInputs = with pkgs; [
	  guile
	  guile-config
	  guile-hall
	];

	shellHook =
	  let
	  	    project = pkgs.writeTextFile {
	      name = ".project";
	      text = ''
((nil . ((project-root . default-directory))))
		'';
		    };
	  in
	    ''
	      cat ${project} > .project
	'';
      };
    }
  );
}
