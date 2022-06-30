{
  description = "plutus-simple-model";

  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    iohk-nix.url = "github:input-output-hk/iohk-nix";
    haskell-nix-extra-hackage.url = "github:mlabs-haskell/haskell-nix-extra-hackage/separate-hackages";
    haskell-nix-extra-hackage.inputs.haskell-nix.follows = "haskell-nix";
    haskell-nix-extra-hackage.inputs.nixpkgs.follows = "nixpkgs";

    cardano-addresses.url = "github:input-output-hk/cardano-addresses/b9f424cc64459a95a2f190a1839ec9bc94cc778c";
    cardano-addresses.flake = false;
    cardano-base.url = "github:input-output-hk/cardano-base/631cb6cf1fa01ab346233b610a38b3b4cba6e6ab";
    cardano-base.flake = false;
    cardano-config.url = "github:input-output-hk/cardano-config/1646e9167fab36c0bff82317743b96efa2d3adaa";
    cardano-config.flake = false;
    cardano-crypto.url = "github:input-output-hk/cardano-crypto/f73079303f663e028288f9f4a9e08bcca39a923e";
    cardano-crypto.flake = false;
    cardano-ledger.url = "github:input-output-hk/cardano-ledger/e290bf8d0ea272a51e9acd10adc96b4e12e00d37";
    cardano-ledger.flake = false;
    cardano-node.url = "github:input-output-hk/cardano-node/95c3692cfbd4cdb82071495d771b23e51840fb0e";
    cardano-node.flake = false;
    cardano-prelude.url = "github:input-output-hk/cardano-prelude/bb4ed71ba8e587f672d06edf9d2e376f4b055555";
    cardano-prelude.flake = false;
    cardano-wallet.url = "github:input-output-hk/cardano-wallet/0cdd1b72a16b2f287b5f1bf137b5eba15bc7f300";
    cardano-wallet.flake = false;
    ekg-forward.url = "github:input-output-hk/ekg-forward/297cd9db5074339a2fb2e5ae7d0780debb670c63";
    ekg-forward.flake = false;
    ekg-json.url = "github:vshabanov/ekg-json/00ebe7211c981686e65730b7144fbf5350462608";
    ekg-json.flake = false;
    flat.url = "github:Quid2/flat/ee59880f47ab835dbd73bea0847dab7869fc20d8";
    flat.flake = false;
    goblins.url = "github:input-output-hk/goblins/cde90a2b27f79187ca8310b6549331e59595e7ba";
    goblins.flake = false;
    hedgehog-extras.url = "github:input-output-hk/hedgehog-extras/967d79533c21e33387d0227a5f6cc185203fe658";
    hedgehog-extras.flake = false;
    hw-aeson.url = "github:haskell-works/hw-aeson/d99d2f3e39a287607418ae605b132a3deb2b753f";
    hw-aeson.flake = false;
    hysterical-screams.url = "github:raduom/hysterical-screams/f3bbd38a19f99de5c8ddc650c94330b2d09a865b";
    hysterical-screams.flake = false;
    iohk-monitoring-framework.url = "github:input-output-hk/iohk-monitoring-framework/066f7002aac5a0efc20e49643fea45454f226caa";
    iohk-monitoring-framework.flake = false;
    io-sim.url = "github:input-output-hk/io-sim/57e888b1894829056cb00b7b5785fdf6a74c3271";
    io-sim.flake = false;
    optparse-applicative.url = "github:input-output-hk/optparse-applicative/7497a29cb998721a9068d5725d49461f2bba0e7a";
    optparse-applicative.flake = false;
    ouroboros-network.url = "github:input-output-hk/ouroboros-network/04245dbd69387da98d3a37de9f400965e922bb0e";
    ouroboros-network.flake = false;
    plutus-apps.url = "github:input-output-hk/plutus-apps/a9763f89f65ec4c17b11a40de37a4824c3a7a201";
    plutus-apps.flake = false;
    plutus.url = "github:input-output-hk/plutus/d24a7540e4659b57ce2ab25dadb968991e232191";
    plutus.flake = false;
    purescript-bridge.url = "github:input-output-hk/purescript-bridge/47a1f11825a0f9445e0f98792f79172efef66c00";
    purescript-bridge.flake = false;
    servant-purescript.url = "github:input-output-hk/servant-purescript/44e7cacf109f84984cd99cd3faf185d161826963";
    servant-purescript.flake = false;
    typed-protocols.url = "github:input-output-hk/typed-protocols/181601bc3d9e9d21a671ce01e0b481348b3ca104";
    typed-protocols.flake = false;
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
          "${inputs.cardano-addresses}/command-line"
          "${inputs.cardano-addresses}/core"
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
          "${inputs.cardano-config}"
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
          "${inputs.cardano-node}/cardano-api"
          "${inputs.cardano-node}/cardano-cli"
          "${inputs.cardano-node}/cardano-git-rev"
          "${inputs.cardano-node}/cardano-node"
          "${inputs.cardano-node}/cardano-submit-api"
          "${inputs.cardano-node}/cardano-testnet"
          "${inputs.cardano-node}/trace-dispatcher"
          "${inputs.cardano-node}/trace-forward"
          "${inputs.cardano-node}/trace-resources"
          "${inputs.cardano-prelude}/cardano-prelude"
          "${inputs.cardano-prelude}/cardano-prelude-test"
          "${inputs.cardano-wallet}/lib/cli"
          "${inputs.cardano-wallet}/lib/core"
          "${inputs.cardano-wallet}/lib/core-integration"
          "${inputs.cardano-wallet}/lib/dbvar"
          "${inputs.cardano-wallet}/lib/launcher"
          "${inputs.cardano-wallet}/lib/numeric"
          "${inputs.cardano-wallet}/lib/shelley"
          "${inputs.cardano-wallet}/lib/strict-non-empty-containers"
          "${inputs.cardano-wallet}/lib/test-utils"
          "${inputs.cardano-wallet}/lib/text-class"
          "${inputs.ekg-forward}"
          "${inputs.ekg-json}"
          "${inputs.flat}"
          "${inputs.goblins}"
          "${inputs.hedgehog-extras}"
          "${inputs.hw-aeson}"
          "${inputs.hysterical-screams}"
          "${inputs.iohk-monitoring-framework}/contra-tracer"
          "${inputs.iohk-monitoring-framework}/iohk-monitoring"
          "${inputs.iohk-monitoring-framework}/plugins/backend-aggregation"
          "${inputs.iohk-monitoring-framework}/plugins/backend-ekg"
          "${inputs.iohk-monitoring-framework}/plugins/backend-monitoring"
          "${inputs.iohk-monitoring-framework}/plugins/backend-trace-forwarder"
          "${inputs.iohk-monitoring-framework}/tracer-transformers"
          "${inputs.io-sim}/io-classes"
          "${inputs.io-sim}/io-sim"
          "${inputs.io-sim}/strict-stm"
          "${inputs.optparse-applicative}"
          "${inputs.ouroboros-network}/monoidal-synchronisation"
          "${inputs.ouroboros-network}/network-mux"
          "${inputs.ouroboros-network}/ntp-client"
          "${inputs.ouroboros-network}/ouroboros-consensus"
          "${inputs.ouroboros-network}/ouroboros-consensus-byron"
          "${inputs.ouroboros-network}/ouroboros-consensus-cardano"
          "${inputs.ouroboros-network}/ouroboros-consensus-protocol"
          "${inputs.ouroboros-network}/ouroboros-consensus-shelley"
          "${inputs.ouroboros-network}/ouroboros-network"
          "${inputs.ouroboros-network}/ouroboros-network-framework"
          "${inputs.ouroboros-network}/ouroboros-network-testing"
          "${inputs.plutus-apps}/doc"
          "${inputs.plutus-apps}/freer-extras"
          "${inputs.plutus-apps}/playground-common"
          "${inputs.plutus-apps}/plutus-chain-index"
          "${inputs.plutus-apps}/plutus-chain-index-core"
          "${inputs.plutus-apps}/plutus-contract"
          "${inputs.plutus-apps}/plutus-contract-certification"
          "${inputs.plutus-apps}/plutus-example"
          "${inputs.plutus-apps}/plutus-ledger"
          "${inputs.plutus-apps}/plutus-ledger-constraints"
          "${inputs.plutus-apps}/plutus-pab"
          "${inputs.plutus-apps}/plutus-pab-executables"
          "${inputs.plutus-apps}/plutus-playground-server"
          "${inputs.plutus-apps}/plutus-script-utils"
          "${inputs.plutus-apps}/plutus-streaming"
          "${inputs.plutus-apps}/plutus-use-cases"
          "${inputs.plutus-apps}/quickcheck-dynamic"
          "${inputs.plutus-apps}/web-ghc"
          "${inputs.plutus}/plutus-core"
          "${inputs.plutus}/plutus-ledger-api"
          "${inputs.plutus}/plutus-tx"
          "${inputs.plutus}/plutus-tx-plugin"
          "${inputs.plutus}/prettyprinter-configurable"
          "${inputs.plutus}/stubs/plutus-ghc-stub"
          "${inputs.plutus}/word-array"
          "${inputs.purescript-bridge}"
          "${inputs.servant-purescript}"
          "${inputs.typed-protocols}/typed-protocols"
          "${inputs.typed-protocols}/typed-protocols-cborg"
          "${inputs.typed-protocols}/typed-protocols-examples"
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
              cardano-wallet-core.components.library.build-tools = [
                pkgs.buildPackages.buildPackages.gitMinimal
              ];
              cardano-config.components.library.build-tools = [
                pkgs.buildPackages.buildPackages.gitMinimal
              ];
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
                  ps.cardano-api
                  ps.cardano-binary
                  ps.cardano-crypto-class
                  ps.cardano-ledger-alonzo
                  ps.cardano-ledger-core
                  ps.cardano-ledger-shelley
                  ps.cardano-ledger-shelley-ma
                  ps.cardano-prelude
                  ps.cardano-slotting
                  ps.generics-sop
                  ps.ouroboros-consensus
                  ps.ouroboros-consensus-shelley
                  ps.plutus-core
                  ps.plutus-ledger
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
