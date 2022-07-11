{
  description = "plutus-simple-model";

  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    iohk-nix.url = "github:input-output-hk/iohk-nix";
    haskell-nix-extra-hackage.url = "github:mlabs-haskell/haskell-nix-extra-hackage/separate-hackages";
    haskell-nix-extra-hackage.inputs.haskell-nix.follows = "haskell-nix";
    haskell-nix-extra-hackage.inputs.nixpkgs.follows = "nixpkgs";

    cardano-base.url = "github:input-output-hk/cardano-base/631cb6cf1fa01ab346233b610a38b3b4cba6e6ab";
    cardano-base.flake = false;
    cardano-crypto.url = "github:input-output-hk/cardano-crypto/f73079303f663e028288f9f4a9e08bcca39a923e";
    cardano-crypto.flake = false;
    cardano-ledger.url = "github:input-output-hk/cardano-ledger/e290bf8d0ea272a51e9acd10adc96b4e12e00d37";
    cardano-ledger.flake = false;
    cardano-prelude.url = "github:input-output-hk/cardano-prelude/bb4ed71ba8e587f672d06edf9d2e376f4b055555";
    cardano-prelude.flake = false;
    flat.url = "github:Quid2/flat/ee59880f47ab835dbd73bea0847dab7869fc20d8";
    flat.flake = false;
    goblins.url = "github:input-output-hk/goblins/cde90a2b27f79187ca8310b6549331e59595e7ba";
    goblins.flake = false;
    plutus.url = "github:input-output-hk/plutus/d24a7540e4659b57ce2ab25dadb968991e232191";
    plutus.flake = false;
    Win32-network.url = "github:input-output-hk/Win32-network/3825d3abf75f83f406c1f7161883c438dac7277d";
    Win32-network.flake = false;
  };

  outputs = { self, nixpkgs, haskell-nix, iohk-nix, haskell-nix-extra-hackage, ... }@inputs:
    let
      defaultSystems = [ "x86_64-linux" "x86_64-darwin" ];

      perSystem = nixpkgs.lib.genAttrs defaultSystems;

      nixpkgsFor = system:
        import nixpkgs {
          overlays = [ haskell-nix.overlay iohk-nix.overlays.crypto ];
          inherit (haskell-nix) config;
          inherit system;
        };

      nixpkgsFor' = system: import nixpkgs { inherit system; };

      ghcVersion = "ghc8107";

      myhackages = system: compiler-nix-name: haskell-nix-extra-hackage.mkHackagesFor system compiler-nix-name (
        [
          "${inputs.cardano-base}/base-deriving-via"
          "${inputs.cardano-base}/binary"
          "${inputs.cardano-base}/binary/test"
          "${inputs.cardano-base}/cardano-crypto-class"
          "${inputs.cardano-base}/cardano-crypto-praos"
          "${inputs.cardano-base}/cardano-crypto-tests"
          "${inputs.cardano-base}/measures"
          "${inputs.cardano-base}/orphans-deriving-via"
          "${inputs.cardano-base}/slotting"
          "${inputs.cardano-base}/strict-containers"
          "${inputs.cardano-crypto}"
          "${inputs.cardano-ledger}/eras/alonzo/impl"
          "${inputs.cardano-ledger}/eras/babbage/impl"
          "${inputs.cardano-ledger}/eras/byron/chain/executable-spec"
          "${inputs.cardano-ledger}/eras/byron/crypto"
          "${inputs.cardano-ledger}/eras/byron/crypto/test"
          "${inputs.cardano-ledger}/eras/byron/ledger/executable-spec"
          "${inputs.cardano-ledger}/eras/byron/ledger/impl"
          "${inputs.cardano-ledger}/eras/byron/ledger/impl/test"
          "${inputs.cardano-ledger}/eras/shelley/impl"
          "${inputs.cardano-ledger}/eras/shelley-ma/impl"
          "${inputs.cardano-ledger}/eras/shelley/test-suite"
          "${inputs.cardano-ledger}/libs/cardano-data"
          "${inputs.cardano-ledger}/libs/cardano-ledger-core"
          "${inputs.cardano-ledger}/libs/cardano-ledger-pretty"
          "${inputs.cardano-ledger}/libs/cardano-protocol-tpraos"
          "${inputs.cardano-ledger}/libs/non-integral"
          "${inputs.cardano-ledger}/libs/set-algebra"
          "${inputs.cardano-ledger}/libs/small-steps"
          "${inputs.cardano-ledger}/libs/small-steps-test"
          "${inputs.cardano-ledger}/libs/vector-map"
          "${inputs.cardano-prelude}/cardano-prelude"
          "${inputs.cardano-prelude}/cardano-prelude-test"
          "${inputs.flat}"
          "${inputs.goblins}"
          "${inputs.plutus}/plutus-core"
          "${inputs.plutus}/plutus-ledger-api"
          "${inputs.plutus}/plutus-tx"
          "${inputs.plutus}/plutus-tx-plugin"
          "${inputs.plutus}/prettyprinter-configurable"
          "${inputs.plutus}/stubs/plutus-ghc-stub"
          "${inputs.plutus}/word-array"
          "${inputs.Win32-network}"
        ]
      );

      haskellModules = [
        ({ pkgs, ... }:
          {
            packages = {
              marlowe.flags.defer-plugin-errors = true;
              plutus-use-cases.flags.defer-plugin-errors = true;
              plutus-ledger.flags.defer-plugin-errors = true;
              plutus-contract.flags.defer-plugin-errors = true;
              cardano-crypto-praos.components.library.pkgconfig = pkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];
              cardano-crypto-class.components.library.pkgconfig = pkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];
            };
          }
        )
      ];

      hackagesFor = system:
        let hackages = myhackages system ghcVersion;
        in {
          inherit (hackages) extra-hackages extra-hackage-tarballs;
          modules = haskellModules ++ hackages.modules;
        };

      projectFor = system:
        let
          pkgs = nixpkgsFor system;
          pkgs' = nixpkgsFor' system;
          hackages = hackagesFor pkgs.system;
          plutus = import inputs.plutus { inherit system; };
          pkgSet = pkgs.haskell-nix.cabalProject' ({
              src = ./.;
              compiler-nix-name = ghcVersion;
              inherit (hackages) extra-hackages extra-hackage-tarballs modules;
              index-state = "2022-05-18T00:00:00Z";
              cabalProjectLocal =
                ''
                  allow-newer:
                      *:aeson
                    , size-based:template-haskell

                  constraints:
                      aeson >= 2
                    , hedgehog >= 1.1
                ''
              ;
              shell = {
                withHoogle = true;
                exactDeps = true;

                nativeBuildInputs = [
                  pkgs'.cabal-install
                  pkgs'.hlint
                  pkgs'.haskellPackages.cabal-fmt
                  pkgs'.nixpkgs-fmt
                ];

                tools.haskell-language-server = { };

                additional = ps: [
                  ps.newtype-generics
                  ps.cardano-crypto-class
                  ps.cardano-ledger-alonzo
                  ps.cardano-ledger-core
                  ps.cardano-ledger-shelley
                  ps.cardano-ledger-shelley-ma
                  ps.cardano-prelude
                  ps.cardano-slotting
                  ps.plutus-core
                  ps.plutus-ledger-api
                  ps.plutus-tx
                  ps.plutus-tx-plugin
                  ps.prettyprinter
                ];
              };
            });
          in pkgSet;
    in {
      flake = perSystem (system: (projectFor system).flake { });

      defaultPackage = perSystem (system:
        let lib = "plutus-simple-model:lib:plutus-simple-model";
        in self.flake.${system}.packages.${lib});

      packages = perSystem (system: self.flake.${system}.packages);

      apps = perSystem (system: self.flake.${system}.apps);

      devShell = perSystem (system: self.flake.${system}.devShell);
      hackages = perSystem (system: hackagesFor system);

      # This will build all of the project's executables and the tests
      check = perSystem (system:
        (nixpkgsFor system).runCommand "combined-check" {
          nativeBuildInputs = builtins.attrValues self.checks.${system}
            ++ builtins.attrValues self.flake.${system}.packages
            ++ [ self.flake.${system}.devShell.inputDerivation ];
        } "touch $out");

      # NOTE `nix flake check` will not work at the moment due to use of
      # IFD in haskell.nix
      #
      # Includes all of the packages in the `checks`, otherwise only the
      # test suite would be included
      checks = perSystem (system: self.flake.${system}.checks);
    };
}
