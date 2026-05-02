{
  description = "A basic flake with a shell";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.naersk.url = "github:nix-community/naersk";
  inputs.rust-overlay.url = "github:oxalica/rust-overlay";

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      naersk,
      rust-overlay,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ (import rust-overlay) ];
        };
        rust = pkgs.rust-bin.selectLatestNightlyWith (
          toolchain:
          toolchain.default.override {
            extensions = [
              "rust-analyzer"
              "rust-src"
            ];
            targets = [ "riscv64gc-unknown-none-elf" ];
          }
        );
        naersk' = pkgs.callPackage naersk {
          cargo = rust;
          rustc = rust;
        };
        types-gdb =
          ps:
          ps.callPackage (
            {
              buildPythonPackage,
              fetchPypi,
              setuptools,
            }:
            buildPythonPackage rec {
              pname = "types_gdb";
              version = "16.3.0.20260408";

              src = fetchPypi {
                inherit pname version;
                hash = "sha256-vfp89d1Jn5Aw+bi7E2gPml15QlJAlCd5FruuVl4f5Fg=";
              };

              postPatch = ''
                substituteInPlace pyproject.toml \
                  --replace-fail "'gdb-stubs' =" "'gdb_stubs' =" \
                  --replace-fail "setuptools>=82.0.1" "setuptools"
              '';

              pyproject = true;
              build-system = [
                setuptools
              ];
            }
          ) { };
      in
      {
        devShell = pkgs.mkShell {
          nativeBuildInputs =
            (with pkgs; [
              rust
              just
              (python3.withPackages (ps: [
                (types-gdb ps)
              ]))
              cargo-binutils
              qemu
              ripgrep-all
              zellij
            ])
            ++ (with pkgs.pkgsCross.riscv64.buildPackages; [
              gcc
            ]);
          RUST_PATH = "${rust}";
          RUST_DOC_PATH = "${rust}/share/doc/rust/html/std/index.html";
          shellHook = ''
            export PANTHEON_DIR=$(realpath .)
            export VVK_BUILD=$PANTHEON_DIR/build
            export RUSTC_SYSROOT=${rust}
            export RA_PROC_MACRO_SRV=${rust}/libexec/rust-analyzer-proc-macro-srv
            export RUST_GDB_PATH=$(dirname $(realpath ${rust}/lib/rustlib/etc/gdb_lookup.py))
            export RUSTC_COMMIT_HASH="$(rustc -vV | sed -n 's/commit-hash: \([a-zA-Z0-9_]*\)/\1/p')"
            export PATH=$VVK_BUILD/release/native/vishvakarma:$PANTHEON_DIR/bin:$PATH
          '';
        };

        packages = {
          types-gdb = types-gdb pkgs.python3Packages;
          vvk = pkgs.stdenv.mkDerivation {
            name = "vvk";

            src = ./.;

            nativeBuildInputs = [ rust ];

            itzamnaQuoteImplTOML = ''
              [package]
              name = "itzamna_quote_impl"
              version = "0.1.0"
              edition = "2024"

              [lib]
              proc-macro = true

              [dependencies]
            '';
            itzamnaQuoteTOML = ''
              [package]
              name = "itzamna_quote"
              version = "0.1.0"
              edition = "2024"

              [workspace]
              members = [".", "impl"]

              [dependencies]
              itzamna_quote_impl = { path = "./impl" }
            '';
            arachneTOML = ''
              [package]
              name = "arachne"
              version = "0.1.0"
              edition = "2024"

              [dependencies]
            '';
            sheshatTOML = ''
              [package]
              name = "sheshat"
              version = "0.1.0"
              edition = "2024"

              [workspace]
              members = [".", "derive"]

              [dependencies]
              sheshat-derive = { path = "./derive" }
            '';
            sheshatDeriveTOML = ''
              [package]
              name = "sheshat-derive"
              version = "0.1.0"
              edition = "2021"

              [lib]
              proc-macro = true

              [dependencies]
              itzamna_quote = { path = "../../itzamna/quote" }
            '';
            vishvakarmaTOML = ''
              [package]
              name = "vishvakarma"
              version = "0.1.0"
              edition = "2024"

              [dependencies]
              sheshat = { path = "../libs/sheshat" }
              arachne = { path = "../libs/arachne" }
            '';

            buildPhase = ''
              mkdir -p libs/itzamna/quote/impl/src
              mv libs/itzamna/{quote_proc.rs,quote/impl/src/lib.rs}
              echo "$itzamnaQuoteImplTOML" > libs/itzamna/quote/impl/Cargo.toml

              mkdir -p libs/itzamna/quote/src
              mv libs/itzamna/{quote.rs,quote/src/lib.rs}
              echo "$itzamnaQuoteTOML" > libs/itzamna/quote/Cargo.toml

              echo "$arachneTOML" > libs/arachne/Cargo.toml
              echo "$sheshatTOML" > libs/sheshat/Cargo.toml
              echo "$sheshatDeriveTOML" > libs/sheshat/derive/Cargo.toml
              echo "$vishvakarmaTOML" > vishvakarma/Cargo.toml

              cd vishvakarma
              cargo build --release
            '';

            installPhase = ''
              mkdir -p $out/bin
              mv target/release/vishvakarma $out/bin/vvk
            '';
          };
        };

        defaultPackage = naersk'.buildPackage ./.;
      }
    );
}
