{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";

    fenix = {
      url = "github:nix-community/fenix/monthly";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      fenix,
    }:
    let
      systems = [
        "x86_64-linux"
      ];

      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f system);

      buildPackage =
        system:
        let
          pkgs = import nixpkgs {
            inherit system;
          };

          pkgsMusl = pkgs.pkgsCross.musl64;

          fenixPkgs = fenix.packages.${system};
        in
        pkgsMusl.callPackage (
          { mkShell }:
          mkShell {
            nativeBuildInputs = [
              (
                with fenixPkgs;
                combine [
                  latest.cargo
                  latest.rustc
                  latest.clippy
                  latest.rustfmt
                  latest.miri
                  targets.x86_64-unknown-linux-musl.latest.rust-std
                ]
              )
            ];

            CARGO_BUILD_TARGET = "x86_64-unknown-linux-musl";
            CARGO_TARGET_X86_64_UNKNOWN_LINUX_GNU_LINKER = "${pkgs.stdenv.cc}/bin/${pkgs.stdenv.cc.targetPrefix}cc";
            CARGO_TARGET_X86_64_UNKNOWN_LINUX_MUSL_LINKER = "${pkgsMusl.stdenv.cc}/bin/${pkgsMusl.stdenv.cc.targetPrefix}cc";
          }
        ) { };
    in
    {
      devShells = forAllSystems (system: {
        default = buildPackage system;
      });
    };
}
