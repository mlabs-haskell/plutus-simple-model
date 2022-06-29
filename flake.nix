{
  description = "template-project";
  nixConfig.bash-prompt = "\\[\\e[0m\\][\\[\\e[0;2m\\]nix-develop \\[\\e[0;1m\\]template-project \\[\\e[0;93m\\]\\w\\[\\e[0m\\]]\\[\\e[0m\\]$ \\[\\e[0m\\]";

  inputs = {
    nixpkgs.follows = "plutip/nixpkgs";
    haskell-nix.follows = "plutip/haskell-nix";

    plutip.url = "github:mlabs-haskell/plutip?rev=88d069d68c41bfd31b2057446a9d4e584a4d2f32";
  };


  outputs = inputs@{ self, nixpkgs, haskell-nix, plutip, ... }:
    let
      # GENERAL
      supportedSystems = with nixpkgs.lib.systems.supported; tier1 ++ tier2 ++ tier3;
      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [ haskell-nix.overlay (import "${plutip.inputs.iohk-nix}/overlays/crypto") ];
        inherit (haskell-nix) config;
      };
      nixpkgsFor' = system: import nixpkgs { inherit system; };

      formatCheckFor = system:
        let
          pkgs = nixpkgsFor system;
          pkgs' = nixpkgsFor' system;
        in
        pkgs.runCommand "format-check"
          {
            nativeBuildInputs = [
              pkgs'.git
              pkgs'.fd
              pkgs'.haskellPackages.cabal-fmt
              pkgs'.nixpkgs-fmt
            ];
          } ''
          export LC_CTYPE=C.UTF-8
          export LC_ALL=C.UTF-8
          export LANG=C.UTF-8
          cd ${self}
          make format_check
          mkdir $out
        ''
      ;

      # OFFCHAIN / Testnet, Cardano, ...

      offchain = rec {
        ghcVersion = "ghc8107";

        projectFor = system:
          let
            pkgs = nixpkgsFor system;
            pkgs' = nixpkgsFor' system;
            fourmolu = pkgs.haskell-nix.tool "ghc921" "fourmolu" { };
            project = pkgs.haskell-nix.cabalProject' {
              src = ./.;
              compiler-nix-name = ghcVersion;
              inherit (plutip) cabalProjectLocal;
              extraSources = plutip.extraSources;
              modules = [
                ({ config, ... }: {
                  packages.template-project-offchain.components.tests.template-project-offchain-test.build-tools = [
                    project.hsPkgs.cardano-cli.components.exes.cardano-cli
                    project.hsPkgs.cardano-node.components.exes.cardano-node
                  ];

                })
              ] ++ plutip.haskellModules;

              shell = {
                withHoogle = true;

                exactDeps = true;

                # We use the ones from Nixpkgs, since they are cached reliably.
                # Eventually we will probably want to build these with haskell.nix.
                nativeBuildInputs = [
                  pkgs'.cabal-install
                  pkgs'.fd
                  pkgs'.haskellPackages.apply-refact
                  pkgs'.haskellPackages.cabal-fmt
                  pkgs'.hlint
                  pkgs'.nixpkgs-fmt

                  project.hsPkgs.cardano-cli.components.exes.cardano-cli
                  project.hsPkgs.cardano-node.components.exes.cardano-node

                  fourmolu
                ];

                tools.haskell-language-server = { };

                additional = ps:
                  with ps; [
                    base-deriving-via
                    cardano-addresses
                    cardano-addresses-cli
                    cardano-binary
                    cardano-crypto
                    cardano-crypto-class
                    cardano-crypto-praos
                    cardano-crypto-wrapper
                    cardano-ledger-alonzo
                    cardano-ledger-byron
                    cardano-ledger-core
                    cardano-ledger-pretty
                    cardano-ledger-shelley
                    cardano-ledger-shelley-ma
                    cardano-node
                    cardano-prelude
                    cardano-slotting
                    flat
                    freer-extras
                    goblins
                    measures
                    orphans-deriving-via
                    plutus-ledger-constraints
                    plutus-core
                    plutus-ledger
                    plutus-ledger-api
                    plutus-tx
                    plutus-tx-plugin
                    prettyprinter-configurable
                    Win32-network
                    word-array
                  ];

              };
            };
          in
          project;
      };
    in
    {
      inherit nixpkgsFor;

      project = perSystem offchain.projectFor;
      flake = perSystem (system: (offchain.projectFor system).flake { });

      # this could be done automatically, but would reduce readability
      packages = perSystem (system: self.flake.${system}.packages);
      checks = perSystem (system: self.flake.${system}.checks);
      check = perSystem (system:
        (nixpkgsFor system).runCommand "combined-test" {
          nativeBuildInputs = builtins.attrValues self.checks.${system};
        } "touch $out");
      apps = perSystem (system: self.flake.${system}.apps);
      devShell = perSystem (system: self.flake.${system}.devShell);
    };
}

