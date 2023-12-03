{
  description = "A Nix-flake-based Haskell development environment";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forEachSupportedSystem = f: nixpkgs.lib.genAttrs supportedSystems (system: f {
        pkgs = import nixpkgs { inherit system; };
      });
    in
      {
	devShells = forEachSupportedSystem ({ pkgs }: let
	  myGhc = (pkgs.ghc.withPackages(hpkgs: with hpkgs; [haskell-language-server HUnit ghci]));
	  in {
        default = pkgs.mkShell {
          packages = with pkgs; [ cabal-install myGhc ];
        };

	INPUT_FILE = "/Users/damienbiasotto/Code/Perso/AoC/2023/01/fixture.txt";

	
	shellHook = let
	  dir-locals = pkgs.writeTextFile {
	    name = ".dir-locals.el";
	    text = ''
            (
  (haskell-ts-mode . ((eglot-workspace-configuration . (:haskell-language-server (:command "${myGhc}/bin/haskell-language-server-wrapper --lsp")))))
)
            '';
	  };
	in
	''
	'';
      });
    };
}
