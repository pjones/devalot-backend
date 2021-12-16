{
  description = "Backend server for devalot.com";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.05";
  };

  outputs = { self, nixpkgs, ... }:
    let
      # List of supported systems:
      supportedSystems = [ "x86_64-linux" ];

      # Function to generate a set based on supported systems:
      forAllSystems = f:
        nixpkgs.lib.genAttrs supportedSystems (system: f system);

      # Attribute set of nixpkgs for each system:
      nixpkgsFor = forAllSystems (system: import nixpkgs { inherit system; });
    in
    {
      packages = forAllSystems
        (system:
          let
            pkgs = nixpkgsFor.${system};
            hlib = pkgs.haskell.lib;
          in
          {
            # Full Haskell package with shared/static libraries:
            lib = import ./. { inherit pkgs; };

            # Just the devalot-backend executable:
            bin = hlib.justStaticExecutables self.packages.${system}.lib;
          });

      defaultPackage =
        forAllSystems (system: self.packages.${system}.bin);

      overlay = final: prev: {
        pjones = (prev.pjones or { }) //
          { devalot-backend = self.packages.${prev.system}.bin; };
      };

      devShell = forAllSystems (system: nixpkgsFor.${system}.mkShell {
        buildInputs = with nixpkgsFor.${system};[
          haskellPackages.cabal-install
          haskellPackages.hlint
        ];

        inputsFrom = builtins.attrValues self.packages.${system};
      });
    };
}
