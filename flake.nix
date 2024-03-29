{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils }:
    utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; };
      in {

        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            cargo
            clippy
            rust-analyzer
            rustc
            rustfmt
          ];
        };

      });
}
