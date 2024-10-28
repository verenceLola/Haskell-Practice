{
  description = "Learning Haskell",
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = import nixpkgs {
            inherit system;
            config.allowUnfree = true;
          };
        in
        with pkgs; {
          devShells.default = mkShell {
            LC_ALL = "C.UTF-8";
            buildInputs = [
              haskell.compiler.native-bignum.ghc982
            ];
          };
        }
    );
}
