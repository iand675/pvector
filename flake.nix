{
  description = "pvector - RRB-tree persistent vectors with O(log n) concat/split";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
    in
    {
      packages = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          pvector = pkgs.haskellPackages.callCabal2nix "pvector" self {};
        in
        {
          inherit pvector;
          default = pvector;
        }
      );

      devShells = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          hspkgs = pkgs.haskellPackages;
          pvector = pkgs.haskell.lib.doBenchmark (hspkgs.callCabal2nix "pvector" self {});
        in
        {
          default = hspkgs.shellFor {
            packages = _: [ pvector ];
            nativeBuildInputs = [
              hspkgs.cabal-install
              hspkgs.haskell-language-server
              hspkgs.fourmolu
              hspkgs.hlint
              pkgs.ghciwatch
            ];
          };
        }
      );
    };
}
