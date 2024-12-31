
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
	    fourmolu
	  ];

	  shellHook =	  let
	    project = pkgs.writeTextFile {
	      name = ".project";
	      text = ''
((nil . ((project-root . default-directory))))
		'';
	    };
	    dir-locals = pkgs.writeTextFile {
	      name = ".dir-locals.el";
	      text = ''
((haskell-ts-mode
  ;; Basic haskell-ts-mode settings
  (haskell-formatter . "fourmolu")  ; or "ormolu", "brittany", "floskell"
  (haskell-process-type . "cabal-repl")
  (haskell-compile-command . "cabal build")
  
  ;; Eglot settings
  (eglot-workspace-configuration
   . (:haskell (:checkProject t
                :formattingProvider "fourmolu"
                :maxCompletions 40
                :plugin (:eval-global :on))))
  
  ;; Cabal specific settings
  (haskell-cabal-dir . (locate-dominating-file default-directory "advent-of-code.cabal"))
  
  
  ;; Project-specific GHC options
  (haskell-ghc-options . ("-Wall"
                         "-Wcompat"
                         "-Widentities"
                         "-Wincomplete-record-updates"
                         "-Wincomplete-uni-patterns"
                         "-Wmissing-export-lists"
                         "-Wmissing-home-modules"
                         "-Wpartial-fields"
                         "-Wredundant-constraints"))
  
  ;; Optional: Directory-specific settings
  (eval . (set (make-local-variable 'compile-command)
               (let ((cabal-dir (locate-dominating-file default-directory "advent-of-code.cabal")))
                 (if cabal-dir
                     (format "cd %s && cabal build" cabal-dir)
                   "cabal build"))))
  
  ;; Optional: HLS settings via eglot
  (eglot-sync-connect . 1)
  (eglot-autoshutdown . t))

 ;; Settings for cabal-mode files
 (cabal-mode
  (tab-width . 2)
  (fill-column . 80)))		
	      '';
	    };
	  in
	  ''
	  gen-hie > hie.yaml
          cat ${project} > .project
	  cat ${dir-locals} > .dir-locals.el
	  '';
	};
      }
  );
}
