{
  description = "AOC - Elixir edition";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils }:
  utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; config.allowUnfree = true; };
    in {
      devShell = pkgs.mkShell {
	buildInputs = with pkgs; [elixir elixir-ls];
	shellHook =
	  let
	    project = pkgs.writeTextFile {
	      name = ".project";
	      text = ''
((nil . ((project-root . default-directory))))
		'';
	    };
	    dir-locals = pkgs.writeTextFile {
	      name = ".dir-locals.el";
	      text = ''
((elixir-ts-mode . ((eglot-workspace-configuration
                     . (:elixirLS (:dialyzerEnabled :json-false
                                  :enableTestLenses t
                                  :fetchDeps t
                                  :mixEnv "dev")))
                    (eglot-server-programs
                     . ((elixir-ts-mode . ("${pkgs.elixir-ls}/bin/elixir-ls"))))
                    (eval . (progn
		    (define-key elixir-ts-mode-map (kbd "C-S-n") #'treesit-beginning-of-defun)
               (define-key elixir-ts-mode-map (kbd "C-S-p") #'treesit-end-of-defun)
                             (eglot-ensure))))))))'';
	    };
	  in 
	    ''
	      cat ${project} > 2024/.project
	      cat ${dir-locals} > 2024/.dir-locals.el
	    '';
      };
    }
  );
}
