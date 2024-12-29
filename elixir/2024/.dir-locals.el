((elixir-ts-mode . ((eglot-workspace-configuration
                     . (:elixirLS (:dialyzerEnabled :json-false
                                  :enableTestLenses t
				  :projectDir (locate-dominating-file default-directory "mix.exs")
                                  :fetchDeps t
                                  :mixEnv "dev")))
                    (eglot-server-programs
                     . ((elixir-ts-mode . ("/nix/store/ichp670bk1hvy0024g5pg7sisskdm89g-elixir-ls-0.25.0/bin/elixir-ls"))))
                    (eval . (progn
		    (define-key elixir-ts-mode-map (kbd "C-S-n") #'treesit-beginning-of-defun)
               (define-key elixir-ts-mode-map (kbd "C-S-p") #'treesit-end-of-defun)
                             (eglot-ensure))))))