{
  description = "A basic flake with a shell";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.naersk.url = "github:nix-community/naersk";
  inputs.rust-overlay.url = "github:oxalica/rust-overlay";

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    naersk,
    rust-overlay,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [(import rust-overlay)];
      };
      rust = pkgs.rust-bin.stable.latest.default.override {
        targets = ["riscv64gc-unknown-none-elf"];
        extensions = ["llvm-tools"];
      };
      naersk' = pkgs.callPackage naersk {
        cargo = rust;
        rustc = rust;
      };
    in {
    devShell = pkgs.mkShell {
        nativeBuildInputs =
          (with pkgs; [
            rust
            just
            python3
            cargo-binutils
          ])
          ++ (with pkgs.pkgsCross.riscv64.buildPackages; [
            gcc
          ]);
        CROSS_COMPILE = "riscv64-unknown-linux-gnu-";
        RUST_PATH = "${rust}";
        RUST_DOC_PATH = "${rust}/share/doc/rust/html/std/index.html";
      };

      defaultPackage = naersk'.buildPackage ./.;
    });
}
