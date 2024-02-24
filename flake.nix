{
  description = "Quandl API V3";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskellPackages;
        packageName = "quandl-api-v3";
      in {
        packages.${packageName} =
          haskellPackages.callCabal2nix packageName self rec {
          };
        defaultPackage = self.packages.${system}.${packageName};
        devShell =
          let libs = [
            pkgs.zlib
          ];
          in
            pkgs.mkShell {
              buildInputs = with pkgs; [
                haskellPackages.haskell-language-server
                cabal-install
                zlib.dev
              ];
              inputsFrom = builtins.attrValues self.packages.${system};
              LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath libs;
          };
      });
}
