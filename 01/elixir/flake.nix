{
  description = "A Nix-flake-based Elixir development environment";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forEachSupportedSystem = f: nixpkgs.lib.genAttrs supportedSystems (system: f {
        pkgs = import nixpkgs { inherit system; };
      });
    in
    {
      devShells = forEachSupportedSystem ({ pkgs }:
	let
        erlang = pkgs.beam.interpreters.erlang_26;
        beamPackages = pkgs.beam.packages.erlang_26;
        elixir = beamPackages.elixir_1_15;
        elixir_ls = beamPackages.elixir-ls;
	in
	{
        default = pkgs.mkShell {
          packages = ([ elixir erlang elixir_ls]) ++
            # Linux only
            pkgs.lib.optionals (pkgs.stdenv.isLinux) (with pkgs; [ gigalixir inotify-tools libnotify ]) ++
            # macOS only
            pkgs.lib.optionals (pkgs.stdenv.isDarwin) (with pkgs; [ terminal-notifier ]) ++
            (with pkgs.darwin.apple_sdk.frameworks; [ CoreFoundation CoreServices ]);
        };

	shellHook =
        let
  dir-locals = pkgs.writeTextFile {
              name = ".dir-locals.el";
              text = ''
(
  (elixir-ts-mode . ((eglot-workspace-configuration . (:elixirLS (:languageServerOverridePath "${elixir_ls}/bin/elixir-ls" :dialyzerEnabled :json-false)))))
  (heex-ts-mode . ((eglot-workspace-configuration . (:elixirLS (:languageServerOverridePath "${elixir_ls}/bin/elixir-ls" :dialyzerEnabled :json-false)))))
)
'';
    };
    in
    ''
      cat ${dir-locals} > .dir-locals.el
    '';

      });
    };
}
