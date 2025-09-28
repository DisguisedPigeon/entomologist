{
  description = "Gleam dev environment for Entomologist";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";
  };

  outputs =
    { nixpkgs, systems, ... }:
    let
      eachSystem = f: nixpkgs.lib.genAttrs (import systems) (system: f nixpkgs.legacyPackages.${system});
    in
    {
      devShells = eachSystem (pkgs: {
        default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # Gleam compiler and tooling
            gleam

            # Erlang stuff
            beamMinimal27Packages.erlang
            beamMinimal27Packages.rebar3
            beamMinimal27Packages.erlfmt
            erlang-language-platform

            # Cool commit emojis helper
            gitmoji-cli

            # Executes a command when a file change is detected
            watchexec
          ]
          # File change notifier. Used by watchexec.
          ++ lib.optional stdenv.isLinux inotify-tools;
        };
      });
    };
}
