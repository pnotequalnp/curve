{
  description = "Discord bot for /r/Cubers";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      hs = pkgs.haskellPackages;
      curve = hs.callCabal2nix "curve" ./. {};
      docker = pkgs.dockerTools.buildImage {
        name = "pnotequalnp/curve";
        tag = "0.1.0.8";
        contents = [ pkgs.bash pkgs.coreutils curve ];
        config.Cmd = [ "curve" ];
      };
    in {
      defaultPackage = curve;
      packages = { inherit docker curve; };
      devShell = curve.env.overrideAttrs (super: {
        nativeBuildInputs = super.nativeBuildInputs ++ [
          hs.cabal-install
        ];
      });
    }
  );
}
