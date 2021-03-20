{
  description = "Simple Discord bot";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hs = pkgs.haskellPackages;
        curve = hs.callCabal2nix "curve" ./. { };
        docker = pkgs.dockerTools.buildImage {
          name = "pnotequalnp/curve";
          tag = "0.1.0.8";
          contents = [ pkgs.bash pkgs.coreutils curve ];
          config.Cmd = [ "curve" ];
        };
      in rec {
        defaultPackage = curve;
        packages = { inherit docker curve; };
        nixosModule = nixosModules.curve;
        nixosModules = { curve = import ./module.nix curve; };
        devShell = curve.env.overrideAttrs (super: {
          nativeBuildInputs = super.nativeBuildInputs ++ [ hs.cabal-install ];
        });
      });
}
