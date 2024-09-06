{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    rustc
    cargo
    pkg-config
    openssl
    haskell.compiler.ghc8107
    haskellPackages.cabal-install
    haskellPackages.aeson
  ];

  shellHook = ''
    export LD_LIBRARY_PATH=$PWD/rust/stock_data/target/release:$LD_LIBRARY_PATH
    echo "Haskell Stock Trader development environment"
    echo "First, build the Rust library:"
    echo "  cd rust/stock_data && cargo build --release && cd ../.."
    echo "Then, build and run the Haskell project:"
    echo "  cabal build"
    echo "  cabal run"
  '';
}
