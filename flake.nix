{
  description = "Simple Discord bot";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      hs = pkgs.haskellPackages;
      curve = hs.callCabal2nix "curve" ./. { };
      docker = pkgs.dockerTools.buildImage {
        name = "pnotequalnp/curve";
        tag = "0.1.0.8";
        contents = [ pkgs.bash pkgs.coreutils curve ];
        config.Cmd = [ "curve" ];
      };
      module = import ./module.nix curve;
    in rec {
      defaultPackage.x86_64-linux = curve;
      packages.x86_64-linux = { inherit docker curve; };
      nixosModule.x86_64-linux = module;
      nixosModules.x86_64-linux = { curve = module; };
      devShell.x86_64-linux = curve.env.overrideAttrs (super: {
        nativeBuildInputs = super.nativeBuildInputs ++ [ hs.cabal-install ];
      });
    };
}
