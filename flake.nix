{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        devShells.default = pkgs.callPackage ./shell.nix { inherit pkgs; };
        packages.default = pkgs.callPackage ./default.nix { inherit pkgs; };
        checks.tests =
          pkgs.haskellPackages.developPackage {
            root = ./.;
          };
      });
}
