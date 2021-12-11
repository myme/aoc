let
  pkgs = import <nixpkgs> {};
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    cargo
    rls
    rust-analyzer
    rustc
    rustfmt
  ];
}
